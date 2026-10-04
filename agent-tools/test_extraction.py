"""Regression tests for missed text, distinct grammars, and lossless extraction."""
import _paths

import json
from pathlib import Path
import unittest

from text_codec import read_script, read_dialogue, credit_lines, dialogue_entries
from text_codec import Codec
from extract_text import extract, dump_script

ROOT = Path(__file__).resolve().parent.parent
ROM = (ROOT/'slime_original.gba').read_bytes()


class ExtractionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.codec = Codec(ROOT)
        cls.records, cls.report = extract(ROM, ROOT)
        cls.by_id = {r['id']: r for r in cls.records}

    def test_all_records_round_trip_original_bytes(self):
        for r in self.records:
            with self.subTest(record=r['id']):
                a, b = int(r['offset'], 16), int(r['end'], 16)
                self.assertEqual(bytes.fromhex(r['original_hex']), ROM[a:b])
                if r['format'] == 'credits':
                    encoded = self.codec.encode_credits(r['tokens'])
                elif r['format'] == 'small':
                    encoded = self.codec.encode_text(r['tokens'][0], True) + b'\0'
                elif r['format'] in ('dialogue', 'plain'):
                    encoded = self.codec.encode(r['tokens'], r['format'])
                else:
                    continue  # fixed-length grids are independently checked below
                self.assertEqual(encoded, ROM[a:b])

    def test_authoritative_banks_nulls_and_coverage(self):
        self.assertEqual(len(self.report['bank_directory']), 125)
        self.assertEqual(self.report['legacy_slots'], 2346)
        self.assertEqual(self.report['null_slots'], [1876,1877,1878,1879,2086])
        for section in self.report['covered_sections']:
            a,b = int(section['start'],16), int(section['end'],16)
            covered = bytearray(b-a)
            for r in self.records:
                lo,hi = max(a,int(r['offset'],16)), min(b,int(r['end'],16))
                if lo < hi: covered[lo-a:hi-a] = b'\1' * (hi-lo)
            for pad in self.report['zero_padding']:
                lo = int(pad['offset'],16);hi = lo+pad['bytes']
                if a <= lo < hi <= b:
                    self.assertEqual(ROM[lo:hi], bytes(hi-lo))
                    covered[lo-a:hi-a] = b'\1' * (hi-lo)
            self.assertTrue(all(covered))

    def test_recovered_truncated_endings_and_missing_slot(self):
        by_index = {i:r for r in self.records for i in r['legacy_indices']}
        for i in (1522,1950,1952,1964,1966,1979,1981,1993,1995,2007,2009):
            tokens = by_index[i]['tokens']
            pos = tokens.index(['DYNAMIC-TEXT',0])
            self.assertTrue(any(type(t) is str for t in tokens[pos+1:]), i)
            self.assertEqual(tokens[-2:], [['SHOW-PROMPT'],['WAIT-INPUT']])
        self.assertTrue(by_index[1982]['tokens'])
        self.assertTrue(all(by_index[i]['tokens'] for i in [1892,1895,1897,1900]))

    def test_new_pools_and_orphans(self):
        self.assertEqual(self.report['formats']['small'], 102)
        self.assertEqual(self.by_id['small-7D1428']['tokens'], ['ドラお　　　　　'])
        self.assertEqual(self.by_id['plain-72A425']['tokens'], ['うりきれ'])
        # This string has NO pointer: the byte-coverage sweep must still find it.
        self.assertEqual(self.by_id['plain-72A3A7']['tokens'], ['キャタピラー'])
        self.assertIn('dialogue-727DAC', self.by_id)
        # Original pointer enters after the name prefix; preserve both views.
        prefix = self.by_id['dialogue-715787']
        self.assertEqual(prefix['tokens'][0], ['NAME','しんぷ'])
        self.assertIn('dialogue-71578C', self.by_id)

    def test_plain_alignment_is_not_dialogue_newline(self):
        plain,_ = self.codec.decode(ROM,0x713F08,'plain')
        self.assertEqual(plain,['はい',['ALIGN'],'いいえ'])
        dialogue,_ = self.codec.decode(ROM,0x713F08,'dialogue')
        self.assertEqual(dialogue,['はい',['NEWLINE'],'いいえ'])
        with self.assertRaises(ValueError): self.codec.encode([['DELAY',3]],'plain')
        with self.assertRaises(ValueError): self.codec.decode(b'\x06\0',0,'plain')

    def test_duplicate_glyphs_keep_exact_identity(self):
        raw = bytes([0xB7,0xF4,1,0x30,1,0x5E,0])
        tokens,_ = self.codec.decode(raw,0)
        self.assertEqual(tokens,['ケ',['GLYPH',0xF4],'六',['GLYPH',0x15E]])
        self.assertEqual(self.codec.encode(tokens),raw)

    def test_fixed_length_name_grids_and_modifier_keys(self):
        for start,count,end in [(0x713F9E,180,0x714054),(0x714054,50,0x714091)]:
            pos,codes = start,[]
            while len(codes)<count:
                code=ROM[pos];pos+=1
                if code==1:code=256+ROM[pos];pos+=1
                codes.append(code)
            self.assertEqual(pos,end)
            self.assertEqual(sum(c in (14,15) for c in codes),4 if count==180 else 0)
        grid=self.by_id['name-grid-713F9E']['tokens']
        self.assertEqual(grid.count(['DAKUTEN']),2)
        self.assertEqual(grid.count(['HANDAKUTEN']),2)

    def test_no_raw_opcodes_in_current_outputs(self):
        for r in self.records:
            self.assertNotIn('"BYTE"',json.dumps(r['tokens']))
            self.assertNotIn('"CONTROL"',json.dumps(r['tokens']))
        working = read_script(ROOT/'text-dumps/gerb.txt')
        self.assertNotIn('"BYTE"',json.dumps(working))
        self.assertNotIn('"CONTROL"',json.dumps(working))

    def test_single_rom_dump_is_reproducible_and_complete(self):
        path = ROOT/'text-dumps/rom.txt'
        self.assertEqual(path.read_text(), dump_script(self.records))
        rows = {r[0]: r for r in read_script(path)}
        self.assertEqual(len(rows), len(self.records))
        self.assertEqual(len(dialogue_entries(list(rows.values()))), 2322)
        for record in self.records:
            key = (record['legacy_indices'] or [int(record['offset'],16)])[0]
            row = rows[key]
            if record['format'] == 'credits':
                self.assertEqual(self.codec.encode_credits(credit_lines(row)),
                                 bytes.fromhex(record['original_hex']))
            elif record['legacy_indices']:
                self.assertEqual(row[1:], record['tokens'])
            else:
                self.assertEqual(row[1][0], record['format'].upper())
                self.assertEqual(row[1][1:], record['tokens'])

    def test_small_font_accepts_english_aliases_and_rejects_empty_speaker(self):
        self.assertEqual(self.codec.encode_text('Hooly',True),
                         self.codec.encode_text('Ｈｏｏｌｙ',True))
        with self.assertRaises(ValueError): self.codec.encode([['NAME','']])

    def test_wrong_rom_fails_closed(self):
        with self.assertRaises(ValueError): extract(b'not the expected ROM',ROOT)


if __name__ == '__main__': unittest.main()
