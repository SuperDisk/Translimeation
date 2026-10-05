"""Targeted interpreter checks for reading pauses versus scene acknowledgements."""
import _paths

import struct
import unittest
from pathlib import Path

from unicorn import UC_HOOK_CODE
from unicorn.arm_const import *
from probe_text_opcodes import Interpreter, SCRIPT
from probe_text_renderer import create_cpu
from text_codec import Codec, read_dialogue
from text_layout import load_layouts, validate_layouts


class PatchedInterpreter(Interpreter):
    def __init__(self):
        super().__init__()
        self.cpu = create_cpu(Path('dist/slime.gba').read_bytes())
        self.cpu.hook_add(UC_HOOK_CODE, self.hook)

    def call(self, address):
        self.cpu.reg_write(UC_ARM_REG_SP, 0x03007000)
        self.cpu.reg_write(UC_ARM_REG_LR, 0x03000001)
        self.cpu.emu_start(address | 1, 0x03000000, count=100000)
        assert self.cpu.reg_read(UC_ARM_REG_PC) == 0x03000000


class ReadingPauseTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.m = PatchedInterpreter()
        cls.codec = Codec()

    def test_page_accepts_input_without_scene_signal_even_with_busy_actors(self):
        m = self.m
        m.reset(self.codec.encode([['PAGE'], ['CLEAR'], 'A', ['CUE']]))
        m.cpu.mem_write(0x02020013, b'\x01')
        m.cpu.mem_write(0x03004278, b'\x20')
        m.cpu.mem_write(0x030044B8, b'\x84')
        m.write(0x124, 100, 2)  # Fast-forward must stop at the reading pause.
        m.step()
        self.assertEqual(m.u32(0x10C), SCRIPT + 2)
        self.assertEqual(m.u8(0x135), 6)
        self.assertTrue(m.u8(0x126) & 4)
        self.assertEqual(m.called('prompt')[-1][2], 1)
        for _ in range(3):
            m.step()
        self.assertEqual(m.u32(0x10C), SCRIPT + 2)
        m.keys(1)
        m.step()
        self.assertFalse(m.u8(0x126) & 4)
        self.assertEqual(m.cpu.mem_read(0x030044B8, 1), b'\x84')
        self.assertEqual(m.called('prompt')[-1][2], 0)
        # CUE retains the original busy gate and then sends exactly the native flag.
        for _ in range(8):
            m.step()
        self.assertEqual(m.cpu.mem_read(0x030044B8, 1), b'\x84')
        m.cpu.mem_write(0x03004278, b'\x00')
        for _ in range(8):
            m.step()
        self.assertEqual(m.cpu.mem_read(0x030044B8, 1), b'\x94')
        self.assertEqual(m.u8(0x126), 0)

    def test_input_mask_and_no_key_are_preserved(self):
        m = self.m
        for key in (0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 0xFC00):
            m.reset(self.codec.encode([['PAGE']]))
            m.step()
            m.keys(key)
            m.step()
            self.assertEqual(bool(m.u8(0x126) & 4), not bool(key & 0xF1), key)

    def test_original_extended_glyphs_and_delay_still_work(self):
        m = self.m
        for code in range(0x100, 0x1B9):
            m.reset(bytes([1, code - 256, 0]))
            m.step()
            self.assertEqual(m.called('glyph')[0][0], code)
            self.assertEqual(m.u32(0x10C), SCRIPT + 2)
        m.reset(bytes([6, 3, 0x24, 0]))
        m.step()
        for _ in range(3):
            m.step(clear_delay=False)
        self.assertFalse(m.called('glyph'))
        m.step()
        self.assertEqual(m.called('glyph')[0][0], 0x24)

    def test_real_scene_wait_is_not_released_by_reading_pages(self):
        m = self.m
        m.reset(self.codec.encode(['A', ['PAGE'], ['CLEAR'], 'B', ['CUE']]))
        m.cpu.mem_write(0x02020013, b'\x01')
        m.call(0x080A0FE8)  # Actual event-0C acknowledgement setup.
        m.keys(1)
        released = False
        for _ in range(50):
            m.step()
            m.cpu.mem_write(0x03006DFC, struct.pack('<I', 0x030044A0))
            m.call(0x080A6BE0)
            if m.cpu.mem_read(0x02020013, 1)[0] & 4:
                self.assertEqual(m.u32(0x10C), SCRIPT + 7)  # Reached native CUE, past PAGE.
                released = True
                break
        self.assertTrue(released)

    def test_layout_registry_guards_cues_and_noninteractive_text(self):
        layouts = load_layouts()
        entries = read_dialogue('text-dumps/gerb.txt')
        validate_layouts(entries, layouts)
        entry = next(row for row in entries if row[0] == 1155)
        broken = [entry[0], *[t for t in entry[1:] if t != ['CUE']]]
        with self.assertRaises(ValueError):
            validate_layouts([broken], layouts)
        for index in (55, 1169, 1170, 1325, 1403):
            with self.assertRaises(ValueError):
                validate_layouts([[index, 'A', ['PAGE']]], layouts)
        with self.assertRaises(ValueError):
            validate_layouts([[9999, 'A']], layouts)


if __name__ == '__main__':
    unittest.main()
