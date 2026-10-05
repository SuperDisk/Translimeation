"""Shared codecs for the ROM's dialogue, plain/menu and credit grammars. Offsets are file offsets, not CPU addresses.

GLYPH preserves the identity of two duplicate table spellings, not an unknown byte.
The engine's 00 terminator is implicit in dialogue/plain token lists.
"""
import re
from pathlib import Path

OPS = {2: ('NEWLINE', 0), 3: ('SCROLL', 0), 4: ('CLEAR', 0),
       6: ('DELAY', 1), 7: ('SHOW-PROMPT', 0), 8: ('WAIT-INPUT', 0),
       9: ('YES-NO', 0), 10: ('OPEN-MENU', 1), 11: ('SWITCH-WINDOW', 0),
       12: ('COLOR', 1), 13: ('DYNAMIC-TEXT', 1), 14: ('PLAYER-NAME', 0),
       15: ('NOP', 0)}
BY_NAME = {name: (code, nargs) for code, (name, nargs) in OPS.items()}
SYMBOLS = set(BY_NAME) | {'NAME', 'GLYPH', 'ALIGN', 'FORCE-NEWLINE', 'WAIT-FOR-A',
                          'CUE', 'PAGE',
                          'CREDITS', 'PLAIN', 'SMALL', 'DIALOGUE', 'NAME-GRID',
                          'DIGIT-TABLE', 'DAKUTEN', 'HANDAKUTEN'}


class Symbol(str):
    pass


def read_script(path):
    source = Path(path).read_text(encoding="utf-8")
    pattern = re.compile(r'\s+|;[^\n]*|"(?:\\[\s\S]|[^"\\])*"|[()]|[^\s()";]+')
    stack, result, pos = [], [], 0
    for match in pattern.finditer(source):
        if match.start() != pos:
            raise ValueError(f"Malformed script at character {pos}")
        pos = match.end()
        token = match[0]
        if token.isspace() or token.startswith(';'):
            continue
        if token == '(':
            stack.append([])
            continue
        if token == ')':
            if not stack:
                raise ValueError("Unmatched closing parenthesis")
            value = stack.pop()
        elif token.startswith('"'):
            value = re.sub(r'\\([\s\S])', r'\1', token[1:-1])
        elif re.fullmatch(r'\d+', token):
            value = int(token)
        elif token.upper() in SYMBOLS | {'BYTE', 'CONTROL'}:
            value = Symbol(token.upper())
        else:
            raise ValueError(f"Unsupported script token {token!r}")
        (stack[-1] if stack else result).append(value)
    if stack or pos != len(source):
        raise ValueError("Unterminated script")
    if any(not isinstance(row, list) or not row or type(row[0]) is not int for row in result):
        raise ValueError("Expected indexed script entries")
    indices = [row[0] for row in result]
    if len(set(indices)) != len(indices):
        raise ValueError("Duplicate script indices")
    return result


def sexp(value):
    if isinstance(value, list):
        return '(' + ' '.join(map(sexp, value)) + ')'
    if isinstance(value, Symbol) or type(value) is int:
        return str(value)
    return '"' + value.replace('\\', '\\\\').replace('"', '\\"') + '"'


# Controls whose execution must be preserved before reflow adds page waits.
EXECUTION = {'SCROLL', 'CLEAR', 'DELAY', 'SHOW-PROMPT', 'WAIT-INPUT',
             'YES-NO', 'OPEN-MENU', 'SWITCH-WINDOW', 'NOP', 'DYNAMIC-TEXT'}


def control_trace(tokens):
    return [t for t in tokens if isinstance(t, list) and t[0] in EXECUTION]


def dialogue_entries(entries):
    return [r for r in entries if 52 <= r[0] <= 2397 and not 1883 <= r[0] <= 1901]


def read_dialogue(path):
    return dialogue_entries(read_script(path))


def validate_dialogue(tokens, codec):
    """Check authoring syntax and cursor resets without consulting source text."""
    codec.encode(tokens)
    for n, token in enumerate(tokens[:-1]):
        if token in (['WAIT-INPUT'], ['WAIT-FOR-A']):
            if tokens[n + 1] not in (['NEWLINE'], ['FORCE-NEWLINE'], ['CLEAR'],
                                     ['SCROLL'], ['SWITCH-WINDOW']):
                raise ValueError('Missing paragraph separator after input wait')


def credit_lines(entry):
    if len(entry) != 2 or not isinstance(entry[1], list) or entry[1][0] != 'CREDITS':
        raise ValueError(f'{entry[0]}: expected a CREDITS record')
    rows = entry[1][1:]
    return [{'tile_row': row, 'x': x, 'tokens': tokens,
             'separator': 0 if n == len(rows) - 1 else 2}
            for n, (row, x, *tokens) in enumerate(rows)]


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
                code = 256 + get() if c == 1 else c
                if kind == 'dialogue' and code == 0x1FE:
                    result.append(['PAGE'])
                else:
                    self.glyph(result, code)
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
            elif t == ['FORCE-NEWLINE']:
                result.append(2)
            elif t == ['WAIT-FOR-A']:
                result.extend([7, 8])
            elif t == ['CUE']:
                result.extend([7, 8])
            elif t == ['PAGE']:
                result.extend([1, 254])
            elif t[0] == 'NAME' and len(t) == 2 and t[1]:
                result.extend(b'\x05' + self.encode_text(t[1], True) + b'\x05')
            else:
                if not t or t[0] not in BY_NAME:
                    raise ValueError(f'Unknown or malformed dialogue command: {t}')
                c, nargs = BY_NAME[t[0]]
                if len(t) != 1 + nargs:
                    raise ValueError(f'Wrong argument count: {t}')
                if any(type(arg) is not int or not 0 <= arg <= 255 for arg in t[1:]):
                    raise ValueError(f'Expected byte arguments: {t}')
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
