"""Regression checks for the corpus repairs, including deliberately broken input."""
import _paths

import copy
import unittest

from text_codec import read_script, read_dialogue
from check_gerb import ROOT, validate, word_boundaries


def readable(tokens):
    return ''.join(t if type(t) is str else
                   ' ' if t[0] == 'NEWLINE' else
                   'Alex' if t[0] == 'PLAYER-NAME' else
                   '10' if t[0] == 'DYNAMIC-TEXT' else '' for t in tokens)


class GerbTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.entries = read_dialogue(ROOT / 'text-dumps/gerb.txt')
        cls.rows = {r[0]: r for r in cls.entries}

    def test_whole_corpus_against_rom(self):
        result = validate()
        self.assertEqual(result['errors'], [])
        self.assertEqual(result['dialogue_entries'], 2267)
        self.assertEqual(result['translated_credit_cards'], 15)

    def test_control_boundaries_do_not_supply_word_spaces(self):
        self.assertEqual(list(word_boundaries(['10', ['COLOR', 0], 'pots'])), [('10', 'pots')])
        self.assertEqual(list(word_boundaries(['10', ['COLOR', 0], ' pots'])), [])
        self.assertEqual(list(word_boundaries([['PLAYER-NAME'], ['COLOR', 0], 'is here'])), [('@', 'is here')])
        self.assertEqual(list(word_boundaries([['PLAYER-NAME'], ['COLOR', 0], "'s pot"])), [])
        self.assertEqual(list(word_boundaries(['one', ['NEWLINE'], 'two'])), [])

    def test_missing_endings_remain_original_japanese(self):
        original = {r[0]: r for r in read_dialogue(ROOT / 'text-dumps/rom.txt')}
        for i in [1522, 1950, 1952, 1964, 1966, 1979, 1981, 1993, 1995, 2007, 2009]:
            with self.subTest(index=i):
                row, source = self.rows[i], original[i]
                counter = ['DYNAMIC-TEXT', 0]
                self.assertEqual(row[row.index(counter) + 1:],
                                 source[source.index(counter) + 1:])
        # The unsupported '#' paragraph also retains the source text, rather
        # than introducing an English replacement word.
        def third_paragraph(row):
            waits = [n for n, t in enumerate(row) if t == ['WAIT-INPUT']]
            return row[waits[1] + 1:waits[2] + 1]
        self.assertEqual(third_paragraph(self.rows[1574]), third_paragraph(original[1574]))

    def test_preview_retains_corrected_spaces_and_counter_endings(self):
        path = ROOT / 'dist/reflowed.txt'
        if not path.exists():
            self.skipTest('Build the preview first')
        preview = {r[0]: r[1:] for r in read_script(path)}
        for i, phrase in [(68, '100 slimes have been'), (1037, '10 pots'),
                          (1222, 'Alex felt'), (2175, 'Alex got 10 gold')]:
            self.assertIn(phrase, readable(preview[i]))
        for i in [1522, 1950, 1952, 1964, 1966, 1979, 1981, 1993, 1995, 2007, 2009]:
            self.assertIn(i, preview)
            self.assertEqual(preview[i][-2:], [['SHOW-PROMPT'], ['WAIT-INPUT']])
            self.assertIn(['DYNAMIC-TEXT', 0], preview[i])
        # Dialogue, switching-window scene, not a reason to wait twice.
        self.assertNotIn(['WAIT-FOR-A'], preview[1155])

    def test_checker_rejects_reintroduced_damage(self):
        damaged = copy.deepcopy(self.rows)
        # Missing word boundary hidden by a color opcode.
        row = damaged[1037]
        n = next(n for n, t in enumerate(row) if type(t) is str and t.startswith(' pots over there.'))
        row[n] = row[n].lstrip()
        # A dynamic substitution still needs its argument.
        row = damaged[1522]
        row[row.index(['DYNAMIC-TEXT', 0])] = ['DYNAMIC-TEXT']
        # Accidental unencodable glyph and malformed speaker label.
        damaged[798].append('#')
        damaged[74][1] = ['NAME']
        # Reveal an unavailable channel in a partial tablet.
        row = damaged[1347]
        n = row.index(['COLOR', 4])
        row[n] = ['COLOR', 1]
        # A wait alone does not reset the cursor.
        row = damaged[491]
        n = row.index(['WAIT-INPUT'])
        del row[n + 1]
        # Color leak across the rest of the message.
        row = damaged[1623]
        row.pop(max(n for n, t in enumerate(row) if t == ['COLOR', 0]))
        errors = validate(entries=list(damaged.values()))['errors']
        for i in [1037, 1522, 798, 74, 1347, 491, 1623]:
            self.assertTrue(any(error.startswith(f'{i}:') for error in errors), (i, errors))


if __name__ == '__main__':
    unittest.main()
