#!/usr/bin/env python3
"""Check the formatting-reviewed corpus against the original ROM and review ledger.

This detects structural/typographic regressions, not mistranslations. Intentional
name-count differences and pending meaning reviews are recorded in the ledger.
Run before audit_text.py and build_preview.lisp. No files are rewritten.
"""
import hashlib
import json
import re
from pathlib import Path

from audit_text import read_script, sexp, pointer, decode_credits
from text_codec import Codec

ROOT = Path(__file__).resolve().parent.parent
TABLETS = range(1346, 1356)
ROM_SHA256 = 'f86a933440369e13a6898864d1ac10b8af409674c489a2ccc9a89cdfa6d2a661'
# These affect execution, as opposed to text layout/color/name substitution.
EXECUTION = {'SCROLL', 'CLEAR', 'DELAY', 'SHOW-PROMPT', 'WAIT-INPUT',
             'YES-NO', 'OPEN-MENU', 'SWITCH-WINDOW', 'NOP', 'DYNAMIC-TEXT'}


def control_trace(tokens):
    return [t for t in tokens if isinstance(t, list) and t[0] in EXECUTION]


def word_boundaries(tokens):
    """Return suspicious English joins, looking through color commands only."""
    previous = None
    for t in tokens:
        if type(t) is str:
            current = t
        elif t[0] in ('PLAYER-NAME', 'DYNAMIC-TEXT'):
            current = '@'
        elif t[0] == 'COLOR':
            continue
        else:
            previous = None
            continue
        if not current:
            continue
        if previous and re.search(r'[A-Za-z0-9@.,!?:;]$', previous) and re.match(r'[A-Za-z0-9@]', current):
            yield previous, current
        previous = current


def colored_glyphs(tokens, codec):
    """Position/ink stream; hidden glyphs retain their identity and advance."""
    color, result = 0, []
    for t in tokens:
        if type(t) is str:
            data = iter(codec.encode_text(t))
            for c in data:
                result.append((256 + next(data) if c == 1 else c, color))
        elif t[0] == 'COLOR':
            color = t[1]
        elif t[0] == 'NEWLINE':
            result.append(('newline', None))
        else:
            raise ValueError(f'Unexpected inscription command: {t}')
    return result


def validate(root=ROOT, entries=None, check_ledger=True):
    rom = (root / 'slime_original.gba').read_bytes()
    codec = Codec(root)
    ledger = json.loads((root / 'text-dumps/professional-formatting-review.json').read_text())
    baseline_path = root / ledger['baseline']
    baseline = {r[0]: r for r in read_script(baseline_path)}
    entries = entries if entries is not None else read_script(root / ledger['script'])
    rows = {r[0]: r for r in entries}
    errors = []

    def require(ok, message):
        if not ok:
            errors.append(message)

    require(hashlib.sha256(rom).hexdigest() == ROM_SHA256, 'Unexpected original ROM')
    require(len(rows) == len(entries), 'Duplicate dialogue index')
    require(rows.keys() == baseline.keys(), 'Professional dialogue entries were lost or added without review')
    if check_ledger:
        require(hashlib.sha256(baseline_path.read_bytes()).hexdigest() == ledger['baseline_sha256'],
                'The historical named baseline changed')
        changes = {c['index']: c for c in ledger['changes']}
        require(len(changes) == len(ledger['changes']), 'Duplicate review record')
        require(set(changes) == {i for i in rows if rows[i] != baseline.get(i)},
                'Update the before/after review ledger for every changed entry')
        for i, change in changes.items():
            require(change['before'] == sexp(baseline[i]), f'{i}: review before-text differs')
            require(i in rows and change['after'] == sexp(rows[i]), f'{i}: review after-text differs')
            require(bool(change['reasons']), f'{i}: edit has no review reason')
    name_differences = []
    for i, *ts in entries:
        try:
            src, _ = codec.decode(rom, pointer(rom, i))
            encoded = codec.encode(ts)
            decoded, end = codec.decode(encoded, 0)
            require(end == len(encoded) and codec.encode(decoded) == encoded, f'{i}: round trip failed')
        except (ValueError, KeyError, IndexError) as exc:
            errors.append(f'{i}: {exc}')
            continue
        require(control_trace(ts) == control_trace(src), f'{i}: execution controls differ from original')
        require(sum(isinstance(t, list) and t[0] == 'NAME' for t in ts) ==
                sum(isinstance(t, list) and t[0] == 'NAME' for t in src), f'{i}: speaker labels lost')
        if ts.count(['PLAYER-NAME']) != src.count(['PLAYER-NAME']):
            name_differences.append(i)
        if i in TABLETS:
            continue
        for left, right in word_boundaries(ts):
            # A localized noun's plural suffix belongs to the same word.
            if i == 341 and (left, right) == ('Elasto Blast', 's'):
                continue
            errors.append(f'{i}: possible missing space: {left!r} + {right!r}')
        color = 0
        for n, t in enumerate(ts):
            if isinstance(t, list) and t[0] == 'COLOR':
                color = t[1]
                require(color < 4, f'{i}: invisible text outside an inscription')
            if t == ['WAIT-INPUT'] and n + 1 < len(ts):
                require(ts[n + 1] in (['NEWLINE'], ['CLEAR'], ['SWITCH-WINDOW'], ['SCROLL']),
                        f'{i}: missing paragraph separator after input wait')
            if type(t) is str:
                require(not re.search(r' {2,}|[\t\r\n]| +[.,!?;:]', t), f'{i}: literal whitespace anomaly: {t!r}')
                if n and ts[n - 1] == ['NEWLINE'] and i != 1298:
                    require(not re.match(r'^[.,!?;:]', t), f'{i}: punctuation detached by a newline')
        require(color == 0, f'{i}: color never reset before end of dialogue')
    require(name_differences == ledger['player_name_count_differences_reviewed'],
            'Unreviewed player-name count differences')

    # Infer each reveal's available ink channels from the ROM, independently
    # of the translator's damaged masks. Compare every glyph and line break.
    full = colored_glyphs(rows[1355][1:], codec)
    require(sum(c == 'newline' for c, _ in full) == 7, 'Tablet must retain eight lines')
    require(all(ink is None or ink < 4 for _, ink in full), 'Full tablet hides text')
    for i in TABLETS:
        candidate = colored_glyphs(rows[i][1:], codec)
        require([c for c, _ in candidate] == [c for c, _ in full], f'{i}: tablet glyph/position drift')
        if i == 1346:
            continue
        original, _ = codec.decode(rom, pointer(rom, i))
        available = {t[1] for t in original if isinstance(t, list) and t[0] == 'COLOR' and t[1] < 4}
        for position, ((glyph, ink), (_, shown)) in enumerate(zip(full, candidate)):
            if glyph != 'newline':
                require(shown == (ink if ink in available else 4), f'{i}: wrong reveal channel at glyph {position}')

    credits = json.loads((root / 'text-dumps/professional-credits.json').read_text())['cards']
    require([c['index'] for c in credits] == list(range(1883, 1902)), 'Credit inventory incomplete')
    ends = (0x13, 0x18, 0x23, 0x3D, 0x66, 0x97, 0xCE, 0x10C, 0x141, 0x1B8)
    for card in credits:
        require(card['original'] == decode_credits(rom, card['index'], codec.big),
                f"Credit {card['index']}: source coordinates/text differ from ROM")
        lines = card['translation']
        if lines is None:
            continue
        encoded = codec.encode_credits(lines)
        decoded, end = codec.credits(encoded, 0)
        require(end == len(encoded) and codec.encode_credits(decoded) == encoded,
                f"Credit {card['index']}: coordinate/text round trip failed")
        bottom = 0
        for line in lines:
            data = codec.encode(line['tokens'], 'plain')
            require(len(data) <= 32, f"Credit {card['index']}: temporary text buffer overflow")
            width = 0
            stream = iter(data[:-1])
            for glyph in stream:
                if glyph == 1:
                    glyph = 256 + next(stream)
                group = next(n for n, e in enumerate(ends) if glyph <= e)
                width += rom[0x713EB8 + 8 * group + 6] + 1
            x = max(0, (30 - (width + 7) // 8) * 4) if line['x'] == 'center' else line['x']
            require(x + width <= 240, f"Credit {card['index']}: line exceeds screen width")
            require(bottom <= line['tile_row'] <= 18, f"Credit {card['index']}: vertical overlap/overflow")
            bottom = line['tile_row'] + 2
    return {'dialogue_entries': len(rows), 'changed_entries': len(ledger['changes']),
            'restored_endings': len(ledger['restored_ending_ids']),
            'translated_credit_cards': sum(c['translation'] is not None for c in credits),
            'meaning_reviews': ledger['translation_meaning_review'], 'errors': errors}


def main():
    result = validate()
    for error in result['errors']:
        print(error)
    print(f"{result['dialogue_entries']} dialogue entries; {result['changed_entries']} reviewed edits; "
          f"{result['restored_endings']} restored endings; {result['translated_credit_cards']} credit cards; "
          f"{len(result['errors'])} formatting errors; {len(result['meaning_reviews'])} separate meaning reviews.")
    raise SystemExit(bool(result['errors']))


if __name__ == '__main__':
    main()
