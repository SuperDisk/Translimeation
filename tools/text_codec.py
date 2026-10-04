"""Shared codecs for the ROM's dialogue, plain/menu and credit grammars. Offsets are file offsets, not CPU addresses.

GLYPH preserves the identity of two duplicate table spellings, not an unknown byte.
The engine's 00 terminator is implicit in dialogue/plain token lists.
"""
from pathlib import Path

OPS = {2: ('NEWLINE', 0), 3: ('SCROLL', 0), 4: ('CLEAR', 0),
       6: ('DELAY', 1), 7: ('SHOW-PROMPT', 0), 8: ('WAIT-INPUT', 0),
       9: ('YES-NO', 0), 10: ('OPEN-MENU', 1), 11: ('SWITCH-WINDOW', 0),
       12: ('COLOR', 1), 13: ('DYNAMIC-TEXT', 1), 14: ('PLAYER-NAME', 0),
       15: ('NOP', 0)}
BY_NAME = {name: (code, nargs) for code, (name, nargs) in OPS.items()}
SYMBOLS = set(BY_NAME) | {'NAME', 'GLYPH', 'ALIGN', 'FORCE-NEWLINE', 'WAIT-FOR-A'}


def read_table(path):
    result = {}
    for line in Path(path).read_text().splitlines():
        code, text = line.split('=', 1)
        result.setdefault(int(code, 16), text)
    return result


class Codec:
    def __init__(self, root=Path('.')):
        self.big = read_table(root / 'SlimeDialog.tbl')
        self.small = read_table(root / 'Slime_Small.tbl')
        self.inverse = {}
        self.small_inverse = {s: c for c, s in self.small.items()}
        for line in (root / 'Slime_Small.tbl').read_text().splitlines():
            c, s = line.split('=', 1)
            self.small_inverse.setdefault(s, int(c, 16))
        for c, s in self.big.items():
            self.inverse.setdefault(s, c)
        # Additional input spellings, keeping the first code for duplicate glyphs.
        for line in (root / 'SlimeDialog.tbl').read_text().splitlines():
            c, s = line.split('=', 1)
            self.inverse.setdefault(s, int(c, 16))

    def glyph(self, result, code):
        s = self.big[code]
        if self.inverse[s] != code:
            result.append(['GLYPH', code])
        elif result and type(result[-1]) is str:
            result[-1] += s
        else:
            result.append(s)

    def decode(self, rom, start, kind='dialogue', limit=None):
        pos, result = start, []
        end = min(len(rom), limit if limit is not None else start + 16384)
        def get():
            nonlocal pos
            if pos >= end:
                raise ValueError(f'Unterminated {kind} at {start:06X}')
            c = rom[pos]
            pos += 1
            return c
        while True:
            c = get()
            if c == 0:
                return result, pos
            if c == 1 or c >= 16:
                self.glyph(result, 256 + get() if c == 1 else c)
            elif kind == 'plain':
                if c != 2:
                    raise ValueError(f'Invalid plain text code {c:02X} at {pos-1:06X}')
                result.append(['ALIGN'])
            elif c == 5:
                name = ''
                while (c := get()) != 5:
                    name += self.small[c]
                result.append(['NAME', name])
            else:
                name, nargs = OPS[c]
                result.append([name] + ([get()] if nargs else []))

    def encode_text(self, text, small=False):
        table = self.small_inverse if small else self.inverse
        keys = sorted(table, key=len, reverse=True)
        result, pos = bytearray(), 0
        while pos < len(text):
            key = next((s for s in keys if text.startswith(s, pos)), None)
            if key is None:
                raise ValueError(f'Unencodable text: {text[pos:]!r}')
            c = table[key]
            result.extend(bytes([c]) if c < 256 else bytes([1, c - 256]))
            pos += len(key)
        return bytes(result)

    def encode(self, tokens, kind='dialogue'):
        result = bytearray()
        for t in tokens:
            if not isinstance(t, (str, list)) or isinstance(t, list) and not t:
                raise ValueError(f'Malformed token: {t}')
            if type(t) is str:
                result.extend(self.encode_text(t))
            elif t[0] == 'GLYPH' and len(t) == 2 and t[1] in self.big:
                c = t[1]
                result.extend(bytes([c]) if c < 256 else bytes([1, c - 256]))
            elif kind == 'plain':
                if t != ['ALIGN']:
                    raise ValueError(f'Invalid plain token {t}')
                result.append(2)
            elif t[0] == 'NAME' and len(t) == 2 and t[1]:
                result.extend(b'\x05' + self.encode_text(t[1], True) + b'\x05')
            else:
                if not t or t[0] not in BY_NAME:
                    raise ValueError(f'Unknown or malformed dialogue command: {t}')
                c, nargs = BY_NAME[t[0]]
                if len(t) != 1 + nargs:
                    raise ValueError(f'Wrong argument count: {t}')
                result.extend([c] + t[1:])
        return bytes(result) + b'\0'

    def credits(self, rom, start):
        pos, lines = start, []
        while rom[pos]:
            y, x = rom[pos:pos+2]
            pos += 2
            begin = pos
            tokens = []
            while rom[pos] not in (0, 2):
                c = rom[pos]
                pos += 1
                if c == 1:
                    c = 256 + rom[pos]
                    pos += 1
                self.glyph(tokens, c)
            lines.append({'tile_row': y, 'x': 'center' if x == 255 else x,
                          'tokens': tokens, 'separator': rom[pos],
                          'encoded_bytes': pos - begin})
            if rom[pos] == 0:
                break
            pos += 1
        return lines, pos + 1

    def encode_credits(self, lines):
        result = bytearray()
        for line in lines:
            result.extend([line['tile_row'], 255 if line['x'] == 'center' else line['x']])
            data = self.encode(line['tokens'], 'plain')[:-1]
            result.extend(data)
            result.append(line['separator'])
        if not lines or lines[-1]['separator'] != 0:
            result.append(0)
        return bytes(result)
