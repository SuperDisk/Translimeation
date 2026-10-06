"""Exercise the font hook and real GBA blitters. Needs local ROM and Unicorn.

Build dist/slime.gba first. No renderer, pixel-copy or lookup boundary is stubbed;
the DMA register hook simulates the hardware transfer in the existing probe.
"""
import _paths

import struct
import unittest
from pathlib import Path

from unicorn.arm_const import *
from unicorn import Uc, UC_ARCH_ARM, UC_MODE_ARM, UC_HOOK_CODE
from probe_text_renderer import create_cpu, draw, reset, descriptor, glyph_pixels, ROM, CONTEXT, DEST
from rocket_font import load_font, import_font, nds_file, ASSET


class RocketFontTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rom = Path('dist/slime.gba').read_bytes()
        cls.cpu = create_cpu(cls.rom)
        cls.glyphs = load_font()

    def expected(self, code, color):
        if code in self.glyphs:
            width, data = self.glyphs[code]
        else:
            ptr, first, width, stride = descriptor(code)
            offset = ptr - 0x08000000 + (code - first) * stride
            data = ROM[offset:offset + stride]
        pixels = [(b >> shift) & 3 for b in data for shift in (6, 4, 2, 0)]
        pixels = [1 if p == 0 or color >= 4 else p + 1 + 3 * color for p in pixels]
        return width, [pixels[y * width:(y + 1) * width] for y in range(16)]

    def spacing(self, code):
        return 0 if code in self.glyphs else 1

    def call(self, address, *args):
        for reg, value in zip((UC_ARM_REG_R0, UC_ARM_REG_R1, UC_ARM_REG_R2, UC_ARM_REG_R3), args):
            self.cpu.reg_write(reg, value)
        self.cpu.reg_write(UC_ARM_REG_SP, 0x03007000)
        self.cpu.reg_write(UC_ARM_REG_LR, 0x03000001)
        self.cpu.emu_start(address | 1, 0x03000000, count=2000000)
        self.assertEqual(self.cpu.reg_read(UC_ARM_REG_PC), 0x03000000)
        return self.cpu.reg_read(UC_ARM_REG_R0)

    def pixels(self, width):
        data = self.cpu.mem_read(DEST, ((width + 7) // 8) * 64)
        return [[(data[x // 8 * 64 + y * 4 + x % 8 // 2] >> (x % 2 * 4)) & 15
                 for x in range(width)] for y in range(16)]

    def test_all_glyphs_alignments_colors_and_japanese_fallback(self):
        # Include widths 2 and 3, previously absent from the Japanese font.
        for code in range(16, 0x1B9):
            for color in (0, 1, 2, 3, 4, 255):
                width, glyph = self.expected(code, color)
                gap = [1] * self.spacing(code)
                for alignment in range(8):
                    reset(self.cpu)
                    x = 0
                    expected = [[] for _ in range(16)]
                    # Real draws establish each ring-buffer alignment.
                    while x % 8 != alignment or x == 0:
                        w, space = self.expected(0x1D, 0)
                        for row, pixels in zip(expected, space):
                            row.extend(pixels)
                        x += draw(self.cpu, 0x1D, x)
                        # Three-pixel DS spaces visit all eight alignments.
                    for row, pixels in zip(expected, glyph):
                        row.extend(gap + pixels)
                    self.assertEqual(draw(self.cpu, code, x, color), width + len(gap))
                    self.assertEqual(self.pixels(x + width + len(gap)), expected, (code, color, alignment))
                reset(self.cpu)
                self.assertEqual(draw(self.cpu, code, 0, color), width)
                self.assertEqual(self.pixels(width), glyph)

    def test_centered_name_entry_cells_and_plain_printer(self):
        for code in self.glyphs:
            width, glyph = self.expected(code, 0)
            reset(self.cpu)
            self.call(0x08096BC8, CONTEXT, code)
            left = (16 - width) // 2
            self.assertEqual(self.pixels(16), [[1] * left + row + [1] * (16 - left - width) for row in glyph])
            reset(self.cpu)
            width, glyph = descriptor(code)[2], glyph_pixels(code, 0)
            raw = bytes([code]) if code < 256 else bytes([1, code - 256])
            self.cpu.mem_write(0x02030000, raw + b'\0')
            tiles = self.call(0x08096C40, DEST, 0x02030000, 0, 0)
            self.assertEqual(tiles, ((width + 1 + 7) // 8) * 2)
            self.assertEqual(self.pixels(width + 1), [[1] + row for row in glyph])

    def test_selector_preserves_callee_saved_registers(self):
        registers = [UC_ARM_REG_R4, UC_ARM_REG_R5, UC_ARM_REG_R6, UC_ARM_REG_R7,
                     UC_ARM_REG_R8, UC_ARM_REG_R9, UC_ARM_REG_R10, UC_ARM_REG_R11]
        for reg in registers:
            self.cpu.reg_write(reg, CONTEXT if reg == UC_ARM_REG_R6 else 0x12345678)
        for code in range(16, 0x1B9):
            ptr = self.call(0x080971EC, 0xABCD0000 | code)
            _, first, width, stride = struct.unpack('<IHBB', self.cpu.mem_read(ptr, 8))
            self.assertEqual(width, self.expected(code, 0)[0])
            self.assertLessEqual(first, code)
            self.assertEqual(stride, width * 4)
        self.assertTrue(all(self.cpu.reg_read(reg) == (CONTEXT if reg == UC_ARM_REG_R6 else 0x12345678)
                            for reg in registers))

    def test_fixed_ui_tile_strips_match_original_rom(self):
        from text_codec import read_script
        original = create_cpu(ROM)
        patched = self.cpu
        # All extracted PLAIN strings, including the pot-breaking results strip.
        try:
            for entry in read_script('text-dumps/rom.txt'):
                if not (len(entry) > 1 and isinstance(entry[1], list) and entry[1][0] == 'PLAIN'):
                    continue
                outputs = []
                for self.cpu in (original, patched):
                    self.cpu.mem_write(DEST, b'\0' * 0x4000)
                    count = self.call(0x080970D0, DEST, 0x08000000 + entry[0])
                    outputs.append((count, bytes(self.cpu.mem_read(DEST, 0x4000))))
                self.assertEqual(outputs[0], outputs[1], hex(entry[0]))
                if entry[0] == 0x713F52:
                    self.assertEqual(outputs[1][0], 72)
        finally:
            self.cpu = patched

    def test_asset_matches_ds_rom(self):
        path = Path('Dragon Quest Heroes - Rocket Slime (USA).nds')
        if not path.exists():
            self.skipTest('Local DS ROM unavailable')
        self.assertEqual(import_font(path), Path(ASSET).read_text())

    def test_yes_no_menu_tiles_match_separately_rendered_choices(self):
        from text_codec import Codec, read_script
        codec = Codec()
        entry = next(r for r in read_script('text-dumps/gerb.txt') if r[0] == 0x713F08)
        tokens = entry[1][1:]
        split = tokens.index(['ALIGN'])
        pointer = struct.unpack_from('<I', self.rom, 0x73840C)[0]
        self.assertGreaterEqual(pointer, 0x08800000)
        self.cpu.mem_write(DEST, b'\x11' * 0x1000)
        count = self.call(0x080970D0, DEST, pointer)
        combined = bytes(self.cpu.mem_read(DEST, count * 32))
        for row, option in enumerate((tokens[:split], tokens[split + 1:])):
            self.cpu.mem_write(0x02030000, codec.encode(option, 'plain'))
            self.cpu.mem_write(DEST, b'\x11' * 0x1000)
            self.call(0x080970D0, DEST, 0x02030000)
            expected = bytes(self.cpu.mem_read(DEST, 5 * 64))
            indices = self.rom[0x7383F0 + row * 6:0x7383F6 + row * 6]
            self.assertEqual(indices[0], 255)  # Cursor column remains blank.
            actual = b''.join(b'\x11' * 64 if n == 255 else combined[n * 64:(n + 1) * 64]
                              for n in indices[1:])
            self.assertEqual(actual, expected)

    def test_spacing_and_pixels_match_actual_ds_blitter(self):
        path = Path('Dragon Quest Heroes - Rocket Slime (USA).nds')
        if not path.exists():
            self.skipTest('Local DS ROM unavailable')
        rom = path.read_bytes()
        offset, _, base, size = struct.unpack_from('<4I', rom, 0x20)
        arm9 = rom[offset:offset + size]
        font = nds_file(rom, 'font_data.bin')
        count = struct.unpack_from('<I', font)[0]
        offset, size = struct.unpack_from('<II', font, 12)
        font = font[4 + count * 8 + offset:4 + count * 8 + offset + size]
        ds = Uc(UC_ARCH_ARM, UC_MODE_ARM)
        ds.mem_map(0x02000000, 0x400000)
        ds.mem_map(0x03000000, 0x1000)
        ds.mem_write(base, arm9)
        ds.mem_write(0x02240000, font)
        # Simulate the SDK memory-copy boundary; run the DS glyph compositor
        # and its cursor/ring-buffer updates unchanged, without DS DMA/ITCM.
        def copy(cpu, address, size, data):
            src, dst, length = (cpu.reg_read(reg) for reg in
                                (UC_ARM_REG_R0, UC_ARM_REG_R1, UC_ARM_REG_R2))
            cpu.mem_write(dst, bytes(cpu.mem_read(src, length)))
            cpu.reg_write(UC_ARM_REG_PC, cpu.reg_read(UC_ARM_REG_LR))
        ds.hook_add(UC_HOOK_CODE, copy, begin=0x02017324, end=0x02017324)
        ds.mem_write(0x022130A0, b'\x11' * 256)
        context = 0x02380000
        reset(self.cpu)
        expected = [[1] * 1024 for _ in range(16)]
        x = 0
        for line in Path(ASSET).read_text().splitlines():
            if line.startswith('#'):
                continue
            code, source, *_ = line.split()
            code, source = int(code, 16), int(source, 16)
            width = arm9[0x021312F8 - base + source]
            pointer = 0x02240000 + source // 16 * 0x800 + source % 16 * 64
            for reg, value in [(UC_ARM_REG_R0, context), (UC_ARM_REG_R1, pointer),
                               (UC_ARM_REG_R2, source), (UC_ARM_REG_R3, 0),
                               (UC_ARM_REG_SP, 0x023FF000), (UC_ARM_REG_LR, 0x03000000)]:
                ds.reg_write(reg, value)
            ds.emu_start(0x020E0364, 0x03000000, count=100000)
            self.assertEqual(ds.reg_read(UC_ARM_REG_PC), 0x03000000)
            self.assertEqual(ds.mem_read(context + 0x26, 1)[0], (x + width) % 8)
            self.assertEqual(draw(self.cpu, code, x), width)
            columns = (x % 8 + width + 7) // 8
            tiles = ds.mem_read(0x022130E0, columns * 64)
            for y in range(16):
                for col in range(columns * 8):
                    byte = tiles[col // 8 * 64 + y // 8 * 32 + y % 8 * 4 + col % 8 // 2]
                    expected[y][x // 8 * 8 + col] = (byte >> (col % 2 * 4)) & 15
            x += width
            self.assertEqual(self.pixels(x), [row[:x] for row in expected], hex(code))


if __name__ == '__main__':
    unittest.main()
