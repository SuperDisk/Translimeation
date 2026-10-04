"""IPS format tests and a complete build in a checkout containing no ROM.

If slime_original.gba is present locally, also compare the IPS result byte for
byte with a fresh run of the original ROM-based injector in a temporary tree.
"""
import _paths

import json
import shutil
import struct
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from text_codec import read_script, read_dialogue
from build_ips import ROOT, IPS_LIMIT, ips_patch, load_profile, validate_entries, verify_profile
from text_codec import Codec

INPUTS = ['slurp.lisp', 'SlimeDialog.tbl', 'Slime_Small.tbl',
          'tools/patch_profile.json', 'tools/build_patch_data.lisp', 'tools/build_ips.py',
          'tools/build_rom.lisp', 'tools/text_codec.py', 'text-dumps/rom.txt',
          'text-dumps/gerb.txt']


def read_ips(data):
    """Independent IPS reader for tests, including standard RLE records."""
    assert data[:5] == b'PATCH'
    pos, records = 5, []
    while True:
        address = data[pos:pos + 3]
        assert len(address) == 3
        pos += 3
        if address == b'EOF':
            assert pos == len(data), 'Unexpected trailing data'
            return records
        length = int.from_bytes(data[pos:pos + 2], 'big')
        assert pos + 2 <= len(data)
        pos += 2
        if length:
            payload = data[pos:pos + length]
            assert len(payload) == length
            pos += length
        else:
            assert pos + 3 <= len(data)
            repeat = int.from_bytes(data[pos:pos + 2], 'big')
            assert repeat > 0
            payload = data[pos + 2:pos + 3] * repeat
            pos += 3
        records.append((int.from_bytes(address, 'big'), payload))


def apply_ips(original, patch):
    result = bytearray(original)
    for offset, payload in read_ips(patch):
        if offset + len(payload) > len(result):
            result.extend(b'\0' * (offset + len(payload) - len(result)))
        result[offset:offset + len(payload)] = payload
    return bytes(result)


class IpsFormatTests(unittest.TestCase):
    def test_known_literal_and_rle_examples(self):
        expected = b'PATCH\x00\x00\x03\x00\x04\x00abcEOF'
        self.assertEqual(ips_patch([(3, b'\0abc')]), expected)
        self.assertEqual(apply_ips(b'12345678', expected), b'123\0abc8')
        self.assertEqual(apply_ips(b'12345', b'PATCH\x00\x00\x02\x00\x00\x00\x03zEOF'), b'12zzz')

    def test_large_records_and_append_preserve_every_byte(self):
        text = bytes(range(256)) * 600
        patch = ips_patch([(100, text)])
        self.assertEqual([len(data) for _, data in read_ips(patch)], [65535, 65535, len(text) - 131070])
        self.assertEqual(apply_ips(b'x' * 100, patch), b'x' * 100 + text)

    def test_merge_only_adjacent_writes(self):
        patch = ips_patch([(5, b'Z'), (2, b'Y'), (1, b'X')])
        self.assertEqual(read_ips(patch), [(1, b'XY'), (5, b'Z')])
        self.assertEqual(apply_ips(b'abcdefg', patch), b'aXYdeZg')

    def test_reject_unrepresentable_or_overlapping_records(self):
        for writes in [[(-1, b'x')], [(0x454f46, b'x')], [(IPS_LIMIT, b'x')],
                       [(IPS_LIMIT - 1, b'xx')], [(1, b'xx'), (2, b'y')], [(1, b'')]]:
            with self.subTest(writes=writes), self.assertRaises(ValueError):
                ips_patch(writes)
        self.assertEqual(read_ips(ips_patch([(IPS_LIMIT - 1, b'x')])), [(IPS_LIMIT - 1, b'x')])


class RomFreeBuildTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not shutil.which('sbcl'):
            raise RuntimeError('Install SBCL to test the ROM-free build')
        cls.temp = tempfile.TemporaryDirectory(prefix='translimeation-test-')
        cls.addClassCleanup(cls.temp.cleanup)
        cls.checkout = Path(cls.temp.name)
        for name in INPUTS:
            target = cls.checkout / name
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ROOT / name, target)
        subprocess.run([sys.executable, 'tools/build_ips.py'], cwd=cls.checkout, check=True)
        cls.patch = (cls.checkout / 'dist/slime-patch.ips').read_bytes()
        cls.report = json.loads((cls.checkout / 'dist/build-report.json').read_text())
        cls.profile, cls.source = load_profile(cls.checkout)

    def test_build_contains_only_pointer_writes_and_contiguous_append(self):
        self.assertFalse(list(self.checkout.rglob('*.gba')))
        self.assertEqual({p.name for p in (self.checkout / 'dist').iterdir()},
                         {'slime-patch.ips', 'build-report.json'})
        base = self.profile['source_rom_size']
        table = self.profile['pointer_table_offset']
        expected_slots = {table + 4 * i + n for i in self.report['injected_entries'] for n in range(4)}
        touched, cursor = set(), base
        for offset, payload in read_ips(self.patch):
            if offset < base:
                self.assertLessEqual(offset + len(payload), base)
                touched.update(range(offset, offset + len(payload)))
            else:
                self.assertEqual(offset, cursor)
                cursor += len(payload)
        self.assertEqual(touched, expected_slots)
        self.assertEqual(cursor, self.report['patched_rom_size'])
        script_ids = {r[0] for r in read_dialogue(self.checkout / 'text-dumps/gerb.txt')}
        included = set(self.report['injected_entries'])
        held = {r['index'] for r in self.report['held_entries']}
        self.assertFalse(included & held)
        self.assertEqual(script_ids, included | held)
        self.assertNotIn(1883, included)  # Credits have another grammar/table.
        self.assertNotIn(1876, included)  # Null pointer slot.

    def test_all_pointers_target_complete_strings_and_lines_fit(self):
        # Apply to a dummy buffer, not a reconstructed game ROM.
        data = apply_ips(bytes(self.profile['source_rom_size']), self.patch)
        base = self.profile['source_rom_size']
        codec = Codec(self.checkout)
        for i in self.report['injected_entries']:
            offset = struct.unpack_from('<I', data, self.profile['pointer_table_offset'] + i * 4)[0] - self.profile['gba_base_address']
            self.assertEqual(offset, base)
            tokens, base = codec.decode(data, offset)
            self.assertEqual(codec.encode(tokens), data[offset:base])
            x, pos = 0, offset
            while data[pos]:
                code = data[pos]
                pos += 1
                if code == 1:
                    code = 256 + data[pos]
                    pos += 1
                if code >= 16:
                    width = next(w for a, b, w in self.profile['font_records'] if a <= code <= b)
                    x += width + bool(x)
                elif code == 5:
                    while data[pos] != 5:
                        pos += 1
                    pos += 1
                elif code in (6, 10, 12, 13):
                    pos += 1
                    if code == 13:
                        x += 40 - (not x)
                elif code == 14:
                    x += 56 - (not x)
                elif code in (2, 3, 4, 11):
                    x = 0
                self.assertLessEqual(x, 208, f'Entry {i} exceeds dialogue width')
        self.assertEqual(base, len(data))

    def test_deterministic_build(self):
        first_report = (self.checkout / 'dist/build-report.json').read_bytes()
        subprocess.run([sys.executable, 'tools/build_ips.py'], cwd=self.checkout, check=True)
        self.assertEqual(self.patch, (self.checkout / 'dist/slime-patch.ips').read_bytes())
        self.assertEqual(first_report, (self.checkout / 'dist/build-report.json').read_bytes())

    def test_source_and_control_validation(self):
        codec = Codec(self.checkout)
        for entry in [[1876, 'null'], [1883, 'credits'], [9999, 'outside'],
                      [1522, ['DYNAMIC-TEXT', 0]], [1325, 'Missing menu argument']]:
            with self.subTest(entry=entry), self.assertRaises(ValueError):
                validate_entries([entry], self.source, codec)
        with self.assertRaises(ValueError):
            verify_profile(b'wrong ROM', self.profile, self.source, codec)
        profile_path = self.checkout / 'tools/patch_profile.json'
        saved = profile_path.read_bytes()
        try:
            bad = dict(self.profile, original_dialogue_sha256='0' * 64)
            profile_path.write_text(json.dumps(bad))
            with self.assertRaises(ValueError):
                load_profile(self.checkout)
        finally:
            profile_path.write_bytes(saved)

    def test_optional_local_rom_matches_normal_injector_exactly(self):
        original_path = ROOT / 'slime_original.gba'
        if not original_path.exists():
            self.skipTest('Optional local ROM comparison; CI does not need a ROM')
        original = original_path.read_bytes()
        verify_profile(original, self.profile, self.source, Codec(self.checkout))
        with tempfile.TemporaryDirectory(prefix='translimeation-rom-check-') as tmp:
            checkout = Path(tmp) / 'checkout'
            shutil.copytree(self.checkout, checkout)
            (checkout / 'slime_original.gba').write_bytes(original)
            subprocess.run(['sbcl', '--script', 'tools/build_rom.lisp'], cwd=checkout, check=True)
            expected = (checkout / 'dist/slime.gba').read_bytes()
        self.assertEqual(apply_ips(original, self.patch), expected)


if __name__ == '__main__':
    unittest.main()
