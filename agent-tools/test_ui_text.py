"""Check injected UI fields with the original machine-code renderer."""
import _paths
import json
import struct
import unittest
from pathlib import Path
from unicorn.arm_const import *
from probe_text_renderer import create_cpu, DEST, ROM
from text_codec import Codec, read_script


class UiTextTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rom = Path('dist/slime.gba').read_bytes()
        cls.cpu = create_cpu(cls.rom)
        cls.profile = json.loads(Path('tools/patch_profile.json').read_text())
        cls.script = {r[0]: r for r in read_script('text-dumps/gerb.txt')}

    def test_all_fields_fit_their_actual_tile_allocations(self):
        for key, layout in self.profile['plain_text'].items():
            if 'columns' not in layout or not layout['pointers']:
                continue
            pointer = struct.unpack_from('<I', self.rom, layout['pointers'][0])[0]
            size = sum(layout['columns']) * 64
            for color in range(4):
                with self.subTest(text=key, color=color):
                    self.cpu.mem_write(DEST, b'\xaa' * (size + 64))
                    for reg, value in ((UC_ARM_REG_R0, DEST), (UC_ARM_REG_R1, pointer),
                                       (UC_ARM_REG_R2, color), (UC_ARM_REG_SP, 0x03007000),
                                       (UC_ARM_REG_LR, 0x03000001)):
                        self.cpu.reg_write(reg, value)
                    self.cpu.emu_start(0x08096C41, 0x03000000, count=2000000)
                    self.assertEqual(self.cpu.reg_read(UC_ARM_REG_PC), 0x03000000)
                    self.assertEqual(self.cpu.reg_read(UC_ARM_REG_R0), size // 32)
                    self.assertEqual(bytes(self.cpu.mem_read(DEST + size, 64)), b'\xaa' * 64)

    def test_relocations_and_authored_glyphs(self):
        codec = Codec()
        def fields(tokens):
            # Padding is generated; all non-padding characters must be retained.
            data = codec.encode(tokens, 'plain')[:-1]
            return [v.strip(bytes([0x1D])) for v in data.split(b'\x02')]
        for key, layout in self.profile['plain_text'].items():
            index = int(key, 16)
            original = self.script[index][1][1:]
            for site in layout['pointers']:
                with self.subTest(text=key, pointer=hex(site)):
                    self.assertEqual(struct.unpack_from('<I', ROM, site)[0], 0x08000000 + index)
                    pointer = struct.unpack_from('<I', self.rom, site)[0] - 0x08000000
                    self.assertGreaterEqual(pointer, len(ROM))
                    tokens, _ = codec.decode(self.rom, pointer, 'plain')
                    if 'glyphs' in layout:
                        self.assertEqual(codec.encode(tokens, 'plain'), codec.encode(original, 'plain'))
                    else:
                        self.assertEqual(fields(tokens)[:-1], fields(original))


if __name__ == '__main__':
    unittest.main()
