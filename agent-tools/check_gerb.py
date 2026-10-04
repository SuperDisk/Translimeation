#!/usr/bin/env python3
"""Check Gerb's working script against the ROM; no historical snapshots required."""
import _paths

import hashlib
import re
from pathlib import Path

from audit_text import read_script, pointer
from text_codec import Codec, dialogue_entries, credit_lines, validate_dialogue

ROOT = Path(__file__).resolve().parent.parent
TABLETS = range(1346, 1356)
ROM_SHA256 = 'f86a933440369e13a6898864d1ac10b8af409674c489a2ccc9a89cdfa6d2a661'


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


def validate(root=ROOT, entries=None):
    rom = (root / 'slime_original.gba').read_bytes()
    codec = Codec(root)
    working = read_script(root / 'text-dumps/gerb.txt')
    entries = entries if entries is not None else dialogue_entries(working)
    rows = {r[0]: r for r in entries}
    errors = []

    def require(ok, message):
        if not ok:
            errors.append(message)

    require(hashlib.sha256(rom).hexdigest() == ROM_SHA256, 'Unexpected original ROM')
    require(len(rows) == len(entries), 'Duplicate dialogue index')
    name_differences = []
    for i, *ts in entries:
        try:
            src, _ = codec.decode(rom, pointer(rom, i))
            validate_dialogue(ts, codec)
            encoded = codec.encode(ts)
            decoded, end = codec.decode(encoded, 0)
            require(end == len(encoded) and codec.encode(decoded) == encoded, f'{i}: round trip failed')
        except (ValueError, KeyError, IndexError) as exc:
            errors.append(f'{i}: {exc}')
            continue
        if ts.count(['PLAYER-NAME']) != src.count(['PLAYER-NAME']):
            name_differences.append(i)
        if i in TABLETS:
            continue
        for left, right in word_boundaries(ts):
            # A localized noun's plural suffix belongs to the same word.
            if i == 341 and left == 'Elasto Blast' and re.match(r's\b', right):
                continue
            errors.append(f'{i}: possible missing space: {left!r} + {right!r}')
        color = 0
        for n, t in enumerate(ts):
            if isinstance(t, list) and t[0] == 'COLOR':
                color = t[1]
                require(color < 4, f'{i}: invisible text outside an inscription')
            if type(t) is str:
                require(not re.search(r' {2,}|[\t\r\n]| +[.,!?;:]', t), f'{i}: literal whitespace anomaly: {t!r}')
                if n and ts[n - 1] == ['NEWLINE'] and i != 1298:
                    require(not re.match(r'^[.,!?;:]', t), f'{i}: punctuation detached by a newline')
        require(color == 0, f'{i}: color never reset before end of dialogue')
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

    credits = [r for r in working if 1883 <= r[0] <= 1901]
    ends = (0x13, 0x18, 0x23, 0x3D, 0x66, 0x97, 0xCE, 0x10C, 0x141, 0x1B8)
    for entry in credits:
        card = {'index': entry[0]}
        lines = credit_lines(entry)
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
    return {'dialogue_entries': len(rows), 'translated_credit_cards': len(credits),
            'player_name_count_differences': name_differences, 'errors': errors}


def main():
    result = validate()
    for error in result['errors']:
        print(error)
    print(f"{result['dialogue_entries']} dialogue entries; {result['translated_credit_cards']} credit cards; "
          f"{len(result['errors'])} formatting errors.")
    raise SystemExit(bool(result['errors']))


if __name__ == '__main__':
    main()
