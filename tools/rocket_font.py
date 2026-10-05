#!/usr/bin/env python3
"""Import Rocket Slime's textbox glyphs; pack their GBA descriptors for IPS builds.

Regenerate the small checked-in asset with: python3 tools/rocket_font.py game.nds
Builds only need rocket-font.txt, not either game ROM or an ARM assembler.
"""
import argparse
import hashlib
import struct
from pathlib import Path

from text_codec import Codec

ROOT = Path(__file__).resolve().parent.parent
ASSET = 'tools/rocket-font.txt'
NDS_SHA256 = '99dc6346b9f66d0a578a7dcb2fe8f8566551e76082f7fd0afa7c2573e804c374'
HOOK = 0x971EC
SPACING_HOOK = 0x96AC6
HOOK_SIZE = 80
ORIGINAL_HOOK = bytes.fromhex('00b50004010c0b1c0922132901d80022')
GRAPHICS = (0x738904, 0x738944, 0x7389A8, 0x738AB0, 0x738D88,
            0x7392A8, 0x73998C, 0x73A224, 0x73ACCC, 0x73B6BC)


def nds_file(rom, wanted):
    """Locate a root NitroFS file through the FNT/FAT, not its cartridge offset."""
    fnt, _, fat, _ = struct.unpack_from('<4I', rom, 0x40)
    offset, file_id, _ = struct.unpack_from('<IHH', rom, fnt)
    pos = fnt + offset
    while rom[pos]:
        size = rom[pos]
        pos += 1
        name = rom[pos:pos + (size & 127)].decode('ascii')
        pos += size & 127
        if size & 128:
            pos += 2
        else:
            if name == wanted:
                start, end = struct.unpack_from('<II', rom, fat + file_id * 8)
                return rom[start:end]
            file_id += 1
    raise ValueError(f'Missing NitroFS file: {wanted}')


def import_font(path, root=ROOT):
    rom = Path(path).read_bytes()
    if hashlib.sha256(rom).hexdigest() != NDS_SHA256:
        raise ValueError('Expected Dragon Quest Heroes - Rocket Slime (USA), ADQE')
    font = nds_file(rom, 'font_data.bin')
    count = struct.unpack_from('<I', font)[0]
    offset, length = struct.unpack_from('<II', font, 12)  # proportional body font
    font = font[4 + count * 8 + offset:4 + count * 8 + offset + length]
    arm9_offset, _, arm9_base, _ = struct.unpack_from('<4I', rom, 0x20)
    # ARM 020E0364 reads this table; 020E0C24 selects the 16x16 tiled glyph.
    widths = rom[arm9_offset + 0x021312F8 - arm9_base:][:0x1E0]
    alphabet = ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    mapping = {char: i for i, char in enumerate(alphabet)}
    mapping.update({'?': 0x40, '!': 0x41, "'": 0x42, ',': 0x43, '.': 0x44,
                    '(': 0x48, ')': 0x49, '+': 0x50, '-': 0x51, '~': 0x53,
                    '"': 0x68})
    codec = Codec(root)
    rows = []
    for char, source in mapping.items():
        code, width = codec.inverse[char], widths[source]
        pixels = []
        for y in range(16):
            for x in range(width):
                pos = ((source // 16) * 0x800 + (source % 16) * 64
                       + (y // 8) * 0x400 + (x // 8) * 32 + (y % 8) * 4 + (x % 8) // 2)
                shade = (font[pos] >> (4 * (x & 1))) & 15
                if not 1 <= shade <= 4:
                    raise ValueError(f'Unexpected DS font palette index {shade}')
                pixels.append(shade - 1)  # DS 1..4 -> GBA packed 0..3
        packed = bytes(sum(pixels[i + j] << (6 - 2 * j) for j in range(4))
                       for i in range(0, len(pixels), 4))
        rows.append((code, source, width, packed))
    return ('# Rocket Slime USA textbox font; source SHA256 ' + NDS_SHA256 + '\n'
            '# GBA glyph / DS glyph / width / packed row-major 16-row 2bpp bitmap\n'
            + ''.join(f'{code:04x} {source:04x} {width} {bits.hex()}\n'
                      for code, source, width, bits in sorted(rows)))


def load_font(root=ROOT):
    glyphs = {}
    for line in (root / ASSET).read_text().splitlines():
        if not line or line.startswith('#'):
            continue
        code, source, width, bits = line.split()
        code, width, bits = int(code, 16), int(width), bytes.fromhex(bits)
        if code in glyphs or not 16 <= code <= 0x1B8 or not 2 <= width <= 13 or len(bits) != width * 4:
            raise ValueError(f'Invalid Rocket Slime glyph: {line}')
        glyphs[code] = (width, bits)
    if not glyphs:
        raise ValueError('Empty Rocket Slime font')
    return glyphs


def font_metrics(profile, glyphs):
    return [[code, code, glyphs[code][0] if code in glyphs else width,
             0 if code in glyphs else 1]
            for start, end, width in profile['font_records'] for code in range(start, end + 1)]


def pack_font(offset, profile, glyphs):
    """Return renderer writes and aligned descriptor/bitmap append at OFFSET."""
    if offset & 3:
        raise ValueError('Font descriptors must be word aligned')
    base = profile['gba_base_address']
    records = bytearray(0x1B9 * 8)
    stock_records = bytearray(len(records))
    bitmaps = bytearray()
    for code in range(0x1B9):
        n = next(n for n, (_, end, _) in enumerate(profile['font_records']) if code <= end)
        first, _, width = profile['font_records'][n]
        pointer = base + GRAPHICS[n]
        struct.pack_into('<IHBB', stock_records, code * 8, pointer, first, width, width * 4)
        if code in glyphs:
            width, data = glyphs[code]
            pointer, first = base + offset + 2 * len(records) + len(bitmaps), code
            bitmaps.extend(data)
        struct.pack_into('<IHBB', records, code * 8, pointer, first, width, width * 4)
    # The compositor keeps its context in r6 (r5 in the centered-cell caller).
    # Only the dialogue context at 02001080 uses DS metrics. Plain UI strings use stack-local contexts and
    # fixed tile slices, so their original glyphs AND spacing must be retained.
    # Selector: r0=(u16 glyph)*8; r1=dialogue/stock table selected by r6/r5; return r0+r1.
    # Spacing helper at HOOK+32: retain stock spacing outside dialogue; otherwise
    # omit the leading pixel only for imported bitmaps (pointers >=08800000).
    # Both routines fit within the original 104-byte selector; no new RAM state.
    hook = (bytes.fromhex(
        '0004400b0f498e4201d08d4201d10e4900e00e4940187047c046c046c046c046'
        '0848864206d15046c00007490858c00d112802d2002a00d0704701480047c046'
        '296b090880100002')
            + struct.pack('<II', base + offset, base + offset + len(records)))
    return [(HOOK, hook), (SPACING_HOOK, bytes.fromhex('00f0a1fb'))], bytes(records + stock_records + bitmaps)


def verify_font_source(rom, profile):
    if rom[HOOK:HOOK + len(ORIGINAL_HOOK)] != ORIGINAL_HOOK:
        raise ValueError('Unexpected original font selector')
    if rom[SPACING_HOOK:SPACING_HOOK + 4] != bytes.fromhex('002a2ed0'):
        raise ValueError('Unexpected original glyph-spacing branch')
    for n, (start, _, width) in enumerate(profile['font_records']):
        actual = struct.unpack_from('<IHBB', rom, 0x713EB8 + n * 8)
        expected = (profile['gba_base_address'] + GRAPHICS[n], start, width, width * 4)
        if actual != expected:
            raise ValueError('Unexpected original font descriptor')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('nds', type=Path)
    parser.add_argument('--output', type=Path, default=ROOT / ASSET)
    args = parser.parse_args()
    args.output.write_text(import_font(args.nds))
    print(args.output)
