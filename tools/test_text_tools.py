"""Audit safety checks and independent verification of the generated preview ROM."""
import struct
import tempfile
import unittest
from pathlib import Path

from audit_text import (read_script, decode, decode_credits, table, pointer,
                        repair_arguments, Symbol, BASE, TABLE)

ROOT = Path(__file__).resolve().parent.parent
ROM = (ROOT / 'slime_original.gba').read_bytes()
BIG = table(ROOT / 'SlimeDialog.tbl')
SMALL = table(ROOT / 'Slime_Small.tbl')


class AuditTests(unittest.TestCase):
    def test_reader_rejects_executable_lisp_and_duplicate_indices(self):
        for text in ['(52 #.(delete-file "x"))', '(52 "a") (52 "b")', '(52 "unterminated)']:
            with tempfile.TemporaryDirectory() as tmp:
                path = Path(tmp) / 'bad.txt'
                path.write_text(text)
                with self.assertRaises(ValueError):
                    read_script(path)

    def test_zero_argument_does_not_terminate_source(self):
        original = decode(ROM, 1522, BIG, SMALL)
        control = original.index([Symbol('DYNAMIC-TEXT'), 0])
        self.assertTrue(any(type(t) is str for t in original[control + 1:]))
        self.assertEqual(original[-2:], [['SHOW-PROMPT'], ['WAIT-INPUT']])

    def test_menu_argument_four_is_not_clear_opcode(self):
        fixed, _ = repair_arguments([['BYTE', 10], ['BYTE', 4]], [['CONTROL', 10, 4]])
        self.assertEqual(fixed, [['CONTROL', 10, 4]])

    def test_null_slot_is_not_text(self):
        with self.assertRaises(ValueError):
            pointer(ROM, 1876)

    def test_credits_coordinates_and_independent_pointer_table(self):
        first = decode_credits(ROM, 1883, BIG)
        self.assertEqual([(x['tile_row'], x['x']) for x in first], [(6, 'center'), (10, 'center')])
        self.assertEqual(struct.unpack_from('<I', ROM, 0x557A30)[0],
                         pointer(ROM, 1883) + BASE)

    def test_preview_changes_only_selected_pointers_and_appends(self):
        preview = ROOT / 'slime-professional-preview.gba'
        if not preview.exists():
            self.skipTest('Run tools/build_preview.lisp first')
        rows = read_script(ROOT / 'text-dumps/preview-reflowed.txt')
        patched = preview.read_bytes()
        changes = set()
        for index, *tokens in rows:
            slot = TABLE + index * 4
            changes.update(range(slot, slot + 4))
            offset = pointer(patched, index)
            self.assertGreaterEqual(offset, len(ROM))
            # Independently walk the encoded stream and check every line width.
            x = 0
            while True:
                code = patched[offset]
                offset += 1
                if code == 0:
                    break
                if code == 1:
                    code = 256 + patched[offset]
                    offset += 1
                if code >= 16:
                    ends = (0x13, 0x18, 0x23, 0x3D, 0x66, 0x97, 0xCE, 0x10C, 0x141, 0x1B8)
                    group = next(i for i, end in enumerate(ends) if code <= end)
                    x += ROM[0x713EB8 + 8 * group + 6] + bool(x)
                elif code == 5:
                    while patched[offset] != 5:
                        offset += 1
                    offset += 1
                elif code in (6, 10, 12, 13):
                    offset += 1
                    if code == 13:
                        x += 40 - (not x)
                elif code == 14:
                    x += 56 - (not x)
                elif code in (2, 3, 4, 11):
                    x = 0
                self.assertLessEqual(x, 208, f'Entry {index} overflows')
        # Compare the full original prefix, masking only authorized pointer slots.
        masked = bytearray(patched[:len(ROM)])
        for pos in changes:
            masked[pos] = ROM[pos]
        self.assertEqual(bytes(masked), ROM)
        self.assertEqual(len(rows), len({row[0] for row in rows}))


if __name__ == '__main__':
    unittest.main()
