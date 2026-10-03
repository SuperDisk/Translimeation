#!/usr/bin/env python3
"""Extract Slime Mori Mori custom-compressed GBA graphics streams.

The decompressor is based on the routine at 08098AC8 in slime-mesen-disasm.txt.
It accepts either ROM offsets or GBA ROM addresses such as 0x081DAC74.
"""

from __future__ import annotations

import argparse
import csv
import math
import struct
import sys
import zlib
from dataclasses import dataclass
from pathlib import Path


KNOWN_ASSET_MODES = (0x02, 0x03, 0x0B, 0x0C, 0x12, 0x13)


class DecodeError(Exception):
    pass


def parse_int(text: str) -> int:
    return int(text, 0)


def parse_modes(text: str) -> set[int] | None:
    text = text.strip().lower()
    if text == "all":
        return None
    if text == "known":
        return set(KNOWN_ASSET_MODES)

    modes = set()
    for part in text.split(","):
        part = part.strip()
        if not part:
            continue
        value = parse_int(part)
        if value < 0 or value > 0xFF:
            raise argparse.ArgumentTypeError(f"mode 0x{value:X} is outside byte range")
        modes.add(value)
    if not modes:
        raise argparse.ArgumentTypeError("empty mode list")
    return modes


def parse_header_byte(text: str) -> int | None:
    if text.strip().lower() in ("any", "none", "all"):
        return None
    value = parse_int(text)
    if value < 0 or value > 0xFF:
        raise argparse.ArgumentTypeError(f"header byte 0x{value:X} is outside byte range")
    return value


def rom_address_to_offset(value: int, rom_size: int) -> int:
    if 0x08000000 <= value < 0x0E000000:
        value -= 0x08000000
    if value < 0 or value >= rom_size:
        raise ValueError(f"offset/address 0x{value:X} is outside the ROM")
    return value


class BitReader:
    """MSB-first bit reader over little-endian 32-bit words."""

    def __init__(self, data: bytes, start: int):
        self.data = data
        self.pos = start
        self.buf = 0
        self.bits = 0

    def read_word(self) -> int:
        if self.pos + 4 > len(self.data):
            raise DecodeError("compressed stream ran past input")
        value = struct.unpack_from("<I", self.data, self.pos)[0]
        self.pos += 4
        return value

    def read(self, count: int) -> int:
        if count < 0 or count > 32:
            raise ValueError(f"invalid bit count {count}")
        if count == 0:
            return 0

        value = 0
        while count:
            if self.bits == 0:
                self.buf = self.read_word()
                self.bits = 32

            take = min(count, self.bits)
            value = (value << take) | (self.buf >> (32 - take))
            self.buf = (self.buf << take) & 0xFFFFFFFF
            self.bits -= take
            count -= take

        return value

    @property
    def consumed(self) -> int:
        return self.pos


class HuffmanReader:
    def __init__(self, br: BitReader, symbol_bits: int):
        self.br = br
        self.symbol_bits = symbol_bits
        self.tree: list[list[int | None]] = [[None, None]]
        self.build()

    def new_node(self) -> int:
        self.tree.append([None, None])
        return len(self.tree) - 1

    def build(self) -> None:
        code = 0
        max_code_len = self.symbol_bits * 2

        for code_len in range(1, max_code_len + 1):
            code <<= 1
            count = self.br.read(self.symbol_bits)

            for _ in range(count):
                node = 0
                for bit_index in range(code_len - 1, 0, -1):
                    bit = (code >> bit_index) & 1
                    child = self.tree[node][bit]
                    if child is None:
                        child = self.new_node()
                        self.tree[node][bit] = child
                    elif child < 0:
                        raise DecodeError("invalid Huffman tree")
                    node = child

                leaf_bit = code & 1
                symbol = self.br.read(self.symbol_bits)
                if self.tree[node][leaf_bit] is not None:
                    raise DecodeError("duplicate Huffman code")
                self.tree[node][leaf_bit] = -symbol - 1
                code += 1

    def read_symbol(self) -> int:
        node = 0
        while True:
            bit = self.br.read(1)
            child = self.tree[node][bit]
            if child is None:
                raise DecodeError("Huffman stream used an unassigned code")
            if child < 0:
                return -child - 1
            node = child


@dataclass
class DistanceTable:
    bases: list[int]
    bits: list[int]

    @classmethod
    def read(cls, br: BitReader, count: int) -> "DistanceTable":
        bases: list[int] = []
        bits: list[int] = []
        base = 1
        for _ in range(count):
            bit_count = br.read(4) + 1
            bases.append(base)
            bits.append(bit_count)
            base += 1 << bit_count
        return cls(bases, bits)

    def distance(self, br: BitReader, index: int) -> int:
        if index < 0 or index >= len(self.bits):
            raise DecodeError(f"distance table index {index} is out of range")
        return self.bases[index] + br.read(self.bits[index])


class Decompressor:
    def __init__(self, data: bytes, offset: int, *, max_output: int | None = None):
        self.data = data
        self.offset = offset
        self.max_output = max_output
        self.out = bytearray()
        self.br: BitReader | None = None
        self.mode = 0
        self.literal_reader = self.read_raw_byte

    def read_raw_byte(self) -> int:
        assert self.br is not None
        return self.br.read(8)

    def read_huffman4_byte(self) -> int:
        assert isinstance(self.literal_reader_state, HuffmanReader)
        high = self.literal_reader_state.read_symbol()
        low = self.literal_reader_state.read_symbol()
        return ((high & 0xF) << 4) | (low & 0xF)

    def read_huffman8_byte(self) -> int:
        assert isinstance(self.literal_reader_state, HuffmanReader)
        return self.literal_reader_state.read_symbol() & 0xFF

    def append_byte(self, value: int) -> None:
        if len(self.out) >= self.out_len:
            return
        self.out.append(value & 0xFF)

    def append_word_le(self, value: int) -> None:
        self.append_byte(value)
        self.append_byte(value >> 8)

    def append_literal_byte(self) -> None:
        self.append_byte(self.literal_reader())

    def append_literal_halfword(self) -> None:
        low = self.literal_reader()
        high = self.literal_reader()
        self.append_byte(low)
        self.append_byte(high)

    def copy_bytes(self, distance: int, count: int) -> None:
        if distance <= 0 or distance > len(self.out):
            raise DecodeError(
                f"invalid byte back-reference distance {distance} at output 0x{len(self.out):X}"
            )
        for _ in range(count):
            if len(self.out) >= self.out_len:
                break
            self.out.append(self.out[-distance])

    def copy_halfwords(self, distance: int, count: int) -> None:
        if distance <= 0 or distance > len(self.out):
            raise DecodeError(
                f"invalid halfword back-reference distance {distance} at output 0x{len(self.out):X}"
            )
        for _ in range(count):
            if len(self.out) >= self.out_len:
                break
            src = len(self.out) - distance
            self.append_byte(self.out[src])
            self.append_byte(self.out[src + 1])

    def read_var_groups(self, group_bits: int) -> int:
        value = 0
        payload_bits = group_bits - 1
        while True:
            group = self.br.read(group_bits)  # type: ignore[union-attr]
            value = (value << payload_bits) + (group >> 1)
            if (group & 1) == 0:
                return value

    def setup_literal_reader(self, literal_mode: int) -> None:
        if literal_mode == 1:
            self.literal_reader_state = HuffmanReader(self.br, 4)  # type: ignore[arg-type]
            self.literal_reader = self.read_huffman4_byte
        elif literal_mode == 2:
            self.literal_reader_state = HuffmanReader(self.br, 8)  # type: ignore[arg-type]
            self.literal_reader = self.read_huffman8_byte
        else:
            self.literal_reader_state = None
            self.literal_reader = self.read_raw_byte

    def decompress(self) -> tuple[bytes, int, int]:
        if self.offset + 4 > len(self.data):
            raise DecodeError("offset is too close to end of input")

        header = struct.unpack_from("<I", self.data, self.offset)[0]
        self.out_len = header >> 8
        if self.out_len == 0:
            raise DecodeError("zero-length output")
        if self.max_output is not None and self.out_len > self.max_output:
            raise DecodeError(
                f"output length 0x{self.out_len:X} exceeds limit 0x{self.max_output:X}"
            )

        self.br = BitReader(self.data, self.offset + 4)
        self.mode = self.br.read(8)

        self.setup_literal_reader((self.mode >> 3) & 0x3)

        body_mode = self.mode & 0x7
        if body_mode == 1:
            self.body_lz_simple()
        elif body_mode == 2:
            self.body_lz_extended()
        elif body_mode == 3:
            self.body_lz_halfword()
        elif body_mode == 4:
            self.body_literal_copy()
        else:
            self.body_lz_rle()

        if len(self.out) != self.out_len:
            if len(self.out) > self.out_len:
                del self.out[self.out_len :]
            else:
                raise DecodeError(
                    f"decoder stopped at 0x{len(self.out):X}, expected 0x{self.out_len:X}"
                )

        self.apply_post_filter((self.mode >> 5) & 0x7)
        return bytes(self.out), self.mode, self.br.consumed

    def body_literal_copy(self) -> None:
        while len(self.out) < self.out_len:
            self.append_literal_byte()

    def body_lz_simple(self) -> None:
        table = DistanceTable.read(self.br, 4)  # type: ignore[arg-type]
        while len(self.out) < self.out_len:
            if self.br.read(1) == 0:  # type: ignore[union-attr]
                self.append_literal_byte()
            else:
                index = self.br.read(2)  # type: ignore[union-attr]
                distance = table.distance(self.br, index)  # type: ignore[arg-type]
                count = self.br.read(4) + 3  # type: ignore[union-attr]
                self.copy_bytes(distance, count)

    def body_lz_extended(self) -> None:
        table = DistanceTable.read(self.br, 7)  # type: ignore[arg-type]
        while len(self.out) < self.out_len:
            if self.br.read(1) == 0:  # type: ignore[union-attr]
                self.append_literal_byte()
                continue

            index = self.br.read(3)  # type: ignore[union-attr]
            if index != 7:
                distance = table.distance(self.br, index)  # type: ignore[arg-type]
                count = self.br.read(4) + 3  # type: ignore[union-attr]
                self.copy_bytes(distance, count)
                continue

            prefix = self.read_var_groups(4)
            if self.br.read(1):  # type: ignore[union-attr]
                index = self.br.read(3)  # type: ignore[union-attr]
                distance = table.distance(self.br, index)  # type: ignore[arg-type]
                count = (prefix << 4) + self.br.read(4) + 3  # type: ignore[union-attr]
                self.copy_bytes(distance, count)
            else:
                for _ in range(prefix + 1):
                    self.append_literal_byte()

    def body_lz_halfword(self) -> None:
        table = DistanceTable.read(self.br, 3)  # type: ignore[arg-type]
        while len(self.out) < self.out_len:
            if self.br.read(1) == 0:  # type: ignore[union-attr]
                self.append_literal_halfword()
                continue

            index = self.br.read(2)  # type: ignore[union-attr]
            if index != 3:
                distance = table.distance(self.br, index) * 2  # type: ignore[arg-type]
                count = self.br.read(3) + 2  # type: ignore[union-attr]
                self.copy_halfwords(distance, count)
                continue

            prefix = self.read_var_groups(3)
            if self.br.read(1):  # type: ignore[union-attr]
                index = self.br.read(2)  # type: ignore[union-attr]
                distance = table.distance(self.br, index) * 2  # type: ignore[arg-type]
                count = (prefix << 3) + self.br.read(3) + 2  # type: ignore[union-attr]
                self.copy_halfwords(distance, count)
            else:
                for _ in range(prefix + 1):
                    self.append_literal_halfword()

    def body_lz_rle(self) -> None:
        table = DistanceTable.read(self.br, 2)  # type: ignore[arg-type]
        while len(self.out) < self.out_len:
            control = self.br.read(2)  # type: ignore[union-attr]

            if control < 2:
                distance = table.distance(self.br, control)  # type: ignore[arg-type]
                count = self.br.read(6) + 3  # type: ignore[union-attr]
                self.copy_bytes(distance, count)
            elif control == 2:
                for _ in range(self.br.read(6) + 1):  # type: ignore[union-attr]
                    self.append_literal_byte()
            else:
                count = self.br.read(6) + 1  # type: ignore[union-attr]
                self.append_byte(self.br.read(8))  # type: ignore[union-attr]
                self.copy_bytes(1, count)

    def apply_post_filter(self, filter_mode: int) -> None:
        if filter_mode in (0, 5, 6, 7):
            return
        if len(self.out) < 4:
            return

        if filter_mode == 1:
            self.filter_nibbles()
        elif filter_mode == 2:
            self.filter_bytes()
        elif filter_mode == 3:
            self.filter_halfwords()
        elif filter_mode == 4:
            self.filter_split_bytes()
        else:
            raise DecodeError(f"unknown post-filter {filter_mode}")

    def get16(self, offset: int) -> int:
        return self.out[offset] | (self.out[offset + 1] << 8)

    def put16(self, offset: int, value: int) -> None:
        self.out[offset] = value & 0xFF
        self.out[offset + 1] = (value >> 8) & 0xFF

    def filter_nibbles(self) -> None:
        carry = 0
        for pos in range(2, len(self.out) - 1, 2):
            word = self.get16(pos)
            a = (word >> 4) + carry
            b = word + a
            c = (word >> 12) + b
            carry = (word >> 8) + c

            out_word = ((a & 0xF) << 4) | (b & 0xF)
            out_word |= (c & 0xF) << 12
            out_word |= (carry & 0xF) << 8
            self.put16(pos, out_word)

    def filter_bytes(self) -> None:
        carry = 0
        for pos in range(2, len(self.out) - 1, 2):
            word = self.get16(pos)
            low = (word + carry) & 0xFF
            carry = (word >> 8) + low
            self.put16(pos, ((carry & 0xFF) << 8) | low)

    def filter_halfwords(self) -> None:
        carry = 0
        for pos in range(2, len(self.out) - 1, 2):
            carry = (carry + self.get16(pos)) & 0xFFFF
            self.put16(pos, carry)

    def filter_split_bytes(self) -> None:
        low_carry = 0
        high_carry = 0
        for pos in range(2, len(self.out) - 1, 2):
            word = self.get16(pos)
            low_carry = (low_carry + word) & 0x00FF
            high_carry = (high_carry + word) & 0xFF00
            self.put16(pos, high_carry | low_carry)


def gba_bgr555_to_rgba(value: int) -> tuple[int, int, int, int]:
    r = value & 0x1F
    g = (value >> 5) & 0x1F
    b = (value >> 10) & 0x1F
    return ((r << 3) | (r >> 2), (g << 3) | (g >> 2), (b << 3) | (b >> 2), 255)


def debug_palette(size: int) -> list[tuple[int, int, int, int]]:
    if size <= 16:
        colors = [
            (0, 0, 0, 255),
            (255, 255, 255, 255),
            (214, 40, 40, 255),
            (247, 127, 0, 255),
            (252, 191, 73, 255),
            (106, 153, 78, 255),
            (42, 157, 143, 255),
            (0, 119, 182, 255),
            (72, 202, 228, 255),
            (67, 97, 238, 255),
            (114, 9, 183, 255),
            (181, 23, 158, 255),
            (255, 112, 150, 255),
            (173, 181, 189, 255),
            (108, 117, 125, 255),
            (52, 58, 64, 255),
        ]
        return colors[:size]

    colors = []
    for index in range(size):
        shade = int(index * 255 / max(1, size - 1))
        colors.append((shade, shade, shade, 255))
    return colors


def load_palette(
    *,
    rom: bytes,
    palette_file: Path | None,
    palette_offset: int | None,
    count: int,
) -> list[tuple[int, int, int, int]]:
    if palette_file is None and palette_offset is None:
        return debug_palette(count)

    if palette_file is not None:
        raw = palette_file.read_bytes()
    else:
        assert palette_offset is not None
        start = rom_address_to_offset(palette_offset, len(rom))
        raw = rom[start : start + count * 2]

    if len(raw) < count * 2:
        raise ValueError("palette data is shorter than requested")

    return [
        gba_bgr555_to_rgba(struct.unpack_from("<H", raw, index * 2)[0])
        for index in range(count)
    ]


def tile_indices_4bpp(data: bytes, tile_offset: int) -> list[int]:
    pixels: list[int] = []
    tile = data[tile_offset : tile_offset + 32]
    for byte in tile:
        pixels.append(byte & 0xF)
        pixels.append(byte >> 4)
    return pixels


def tile_indices_8bpp(data: bytes, tile_offset: int) -> list[int]:
    return list(data[tile_offset : tile_offset + 64])


def render_tiles(
    data: bytes,
    *,
    bpp: int,
    tiles_per_row: int,
    palette: list[tuple[int, int, int, int]],
    transparent_index: int | None,
) -> tuple[int, int, bytes]:
    tile_size = 32 if bpp == 4 else 64
    tile_count = len(data) // tile_size
    if tile_count == 0:
        raise ValueError("not enough data for one tile")

    rows = math.ceil(tile_count / tiles_per_row)
    width = tiles_per_row * 8
    height = rows * 8
    image = bytearray(width * height * 4)

    read_tile = tile_indices_4bpp if bpp == 4 else tile_indices_8bpp

    for tile_index in range(tile_count):
        tile_x = (tile_index % tiles_per_row) * 8
        tile_y = (tile_index // tiles_per_row) * 8
        pixels = read_tile(data, tile_index * tile_size)

        for y in range(8):
            for x in range(8):
                color_index = pixels[y * 8 + x]
                color = palette[color_index % len(palette)]
                if transparent_index is not None and color_index == transparent_index:
                    color = (0, 0, 0, 0)
                dst = ((tile_y + y) * width + tile_x + x) * 4
                image[dst : dst + 4] = bytes(color)

    return width, height, bytes(image)


def write_png_rgba(path: Path, width: int, height: int, rgba: bytes) -> None:
    def chunk(kind: bytes, payload: bytes) -> bytes:
        crc = zlib.crc32(kind)
        crc = zlib.crc32(payload, crc) & 0xFFFFFFFF
        return struct.pack(">I", len(payload)) + kind + payload + struct.pack(">I", crc)

    stride = width * 4
    scanlines = bytearray()
    for y in range(height):
        scanlines.append(0)
        start = y * stride
        scanlines.extend(rgba[start : start + stride])

    png = bytearray(b"\x89PNG\r\n\x1a\n")
    png.extend(chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0)))
    png.extend(chunk(b"IDAT", zlib.compress(bytes(scanlines), 9)))
    png.extend(chunk(b"IEND", b""))
    path.write_bytes(png)


def output_stem_for_offset(offset: int) -> str:
    return f"{0x08000000 + offset:08X}"


def shannon_entropy(data: bytes) -> float:
    if not data:
        return 0.0
    counts = [0] * 256
    for byte in data:
        counts[byte] += 1
    length = len(data)
    return -sum((count / length) * math.log2(count / length) for count in counts if count)


def write_outputs(
    args: argparse.Namespace,
    rom: bytes,
    offset: int,
    out: bytes,
    mode: int,
    consumed: int,
    candidate_index: int | None = None,
) -> dict[str, str | int]:
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    prefix = f"{candidate_index:06d}_" if args.prefix_index and candidate_index is not None else ""
    stem = f"{prefix}{output_stem_for_offset(offset)}"
    raw_path = out_dir / f"{stem}.bin"
    png_path = out_dir / f"{stem}_{args.bpp}bpp.png"
    entropy = shannon_entropy(out)

    raw_path.write_bytes(out)

    png_status = ""
    if not args.skip_png:
        try:
            palette_count = 16 if args.bpp == 4 else 256
            palette = load_palette(
                rom=rom,
                palette_file=Path(args.palette_file) if args.palette_file else None,
                palette_offset=args.palette_offset,
                count=palette_count,
            )
            width, height, rgba = render_tiles(
                out,
                bpp=args.bpp,
                tiles_per_row=args.tiles_per_row,
                palette=palette,
                transparent_index=args.transparent_index,
            )
            write_png_rgba(png_path, width, height, rgba)
            png_status = str(png_path)
        except Exception as exc:
            png_status = f"PNG failed: {exc}"

    print(
        f"{stem}: mode=0x{mode:02X} out=0x{len(out):X} "
        f"entropy={entropy:.3f} stream_end=0x{consumed:X} wrote {raw_path}"
        + (f" {png_path}" if png_status == str(png_path) else "")
    )

    return {
        "candidate_index": "" if candidate_index is None else candidate_index,
        "address": f"0x{0x08000000 + offset:08X}",
        "offset": f"0x{offset:X}",
        "file_stem": stem,
        "header_byte": f"0x{rom[offset]:02X}",
        "mode": f"0x{mode:02X}",
        "body_mode": mode & 0x07,
        "literal_mode": (mode >> 3) & 0x03,
        "filter_mode": (mode >> 5) & 0x07,
        "output_size": len(out),
        "entropy": f"{entropy:.4f}",
        "stream_end": f"0x{0x08000000 + consumed:08X}",
        "compressed_size": consumed - offset,
        "raw_path": str(raw_path),
        "png_path": png_status,
    }


def extract_one(args: argparse.Namespace, rom: bytes, offset: int) -> None:
    out, mode, consumed = Decompressor(
        rom, offset, max_output=args.max_output
    ).decompress()
    write_outputs(args, rom, offset, out, mode, consumed)


def write_manifest(path: Path, rows: list[dict[str, str | int]]) -> None:
    if not rows:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def scan(args: argparse.Namespace, rom: bytes) -> None:
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    allowed_modes = args.scan_modes
    rows: list[dict[str, str | int]] = []
    found = 0

    for offset in range(args.scan_start, min(args.scan_end, len(rom) - 8), args.scan_step):
        header = struct.unpack_from("<I", rom, offset)[0]
        header_byte = header & 0xFF
        out_len = header >> 8

        if args.header_byte is not None and header_byte != args.header_byte:
            continue
        if out_len < args.min_output:
            continue
        if args.max_output is not None and out_len > args.max_output:
            continue
        if args.aligned_output and out_len % (32 if args.bpp == 4 else 64) != 0:
            continue

        mode = rom[offset + 7]
        if allowed_modes is not None and mode not in allowed_modes:
            continue

        try:
            out, mode, consumed = Decompressor(
                rom, offset, max_output=args.max_output
            ).decompress()
        except DecodeError:
            continue
        except Exception:
            continue

        if len(out) < args.min_output:
            continue
        if args.aligned_output and len(out) % (32 if args.bpp == 4 else 64) != 0:
            continue

        entropy = shannon_entropy(out)
        if args.min_entropy is not None and entropy < args.min_entropy:
            continue
        if args.max_entropy is not None and entropy > args.max_entropy:
            continue

        if args.scan_limit and found >= args.scan_limit:
            print(f"scan limit {args.scan_limit} reached")
            break
        candidate_index = found
        found += 1

        try:
            rows.append(write_outputs(args, rom, offset, out, mode, consumed, candidate_index))
        except Exception as exc:
            print(f"{output_stem_for_offset(offset)}: decoded but PNG failed: {exc}")

    if not args.no_manifest:
        manifest = Path(args.manifest) if args.manifest else out_dir / "manifest.csv"
        write_manifest(manifest, rows)
        if rows:
            print(f"wrote manifest {manifest}")
    print(f"scan found {found} candidate stream(s)")


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Decompress Slime Mori Mori graphics streams and render GBA tiles as PNG."
    )
    parser.add_argument("rom", help="GBA ROM path")
    parser.add_argument(
        "-o", "--offset", type=parse_int, action="append", default=[],
        help="ROM offset or GBA address to extract, e.g. 0x1DAC74 or 0x081DAC74",
    )
    parser.add_argument("--out-dir", default="extracted-gfx", help="output directory")
    parser.add_argument("--bpp", type=int, choices=(4, 8), default=4, help="tile format")
    parser.add_argument("--tiles-per-row", type=int, default=16)
    parser.add_argument("--palette-file", help="raw GBA BGR555 palette file")
    parser.add_argument("--palette-offset", type=parse_int, help="ROM offset/address of palette")
    parser.add_argument("--skip-png", action="store_true", help="only write raw .bin files")
    parser.add_argument(
        "--prefix-index", action="store_true",
        help="prefix scan output filenames with a zero-padded candidate index",
    )
    parser.add_argument(
        "--transparent-index", type=int,
        help="make this color index transparent in the PNG",
    )
    parser.add_argument(
        "--max-output", type=parse_int, default=0x20000,
        help="refuse streams that expand beyond this many bytes",
    )

    parser.add_argument("--scan", action="store_true", help="try every aligned offset")
    parser.add_argument("--scan-start", type=parse_int, default=0)
    parser.add_argument("--scan-end", type=parse_int, default=0x100000000)
    parser.add_argument("--scan-step", type=parse_int, default=4)
    parser.add_argument(
        "--scan-limit", type=int, default=0,
        help="maximum number of candidates to extract; 0 means no limit",
    )
    parser.add_argument("--min-output", type=parse_int, default=0x80)
    parser.add_argument(
        "--header-byte", type=parse_header_byte, default=0x70,
        help="required low header byte for scanning, or 'any'",
    )
    parser.add_argument(
        "--scan-modes", type=parse_modes, default=set(KNOWN_ASSET_MODES),
        help="comma-separated mode bytes, 'known', or 'all'",
    )
    parser.add_argument(
        "--min-entropy", type=float,
        help="discard decoded candidates below this byte-entropy value",
    )
    parser.add_argument(
        "--max-entropy", type=float,
        help="discard decoded candidates above this byte-entropy value",
    )
    parser.add_argument(
        "--aligned-output", action="store_true",
        help="only keep candidates whose output length is a whole number of tiles",
    )
    parser.add_argument("--manifest", help="CSV manifest path for scan results")
    parser.add_argument("--no-manifest", action="store_true", help="do not write a scan manifest")
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    if args.tiles_per_row <= 0:
        parser.error("--tiles-per-row must be positive")

    rom_path = Path(args.rom)
    rom = rom_path.read_bytes()

    if args.scan:
        args.scan_start = rom_address_to_offset(args.scan_start, len(rom)) if args.scan_start else 0
        args.scan_end = min(
            rom_address_to_offset(args.scan_end, len(rom)) + 1
            if 0x08000000 <= args.scan_end < 0x0E000000
            else args.scan_end,
            len(rom),
        )
        scan(args, rom)

    failures = 0
    for value in args.offset:
        try:
            offset = rom_address_to_offset(value, len(rom))
            extract_one(args, rom, offset)
        except (DecodeError, ValueError, OSError) as exc:
            failures += 1
            print(f"0x{value:X}: {exc}", file=sys.stderr)

    if not args.scan and not args.offset:
        parser.error("provide --offset or --scan")

    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
