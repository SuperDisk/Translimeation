#!/usr/bin/env python3
"""Audit the human translation against the actual ROM, without evaluating Lisp.

The repaired output is a PARTIAL dialogue preview input. Entries requiring prose
or layout review are omitted, leaving their original ROM pointers untouched.
"""
import argparse
import hashlib
import json
import re
import struct
from pathlib import Path
from text_codec import OPS, SYMBOLS, BY_NAME

BASE = 0x08000000
TABLE = 0x71174C
FIRST, LAST = 52, 2397
CREDITS = range(1883, 1902)


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


def table(path):
    result = {}
    for line in Path(path).read_text(encoding="utf-8").splitlines():
        code, text = line.split('=', 1)
        # Use the first spelling for readable Japanese source. Encoding aliases
        # are read separately; duplicate spellings do not affect the audit.
        result.setdefault(int(code, 16), text)
    return result


def pointer(rom, index):
    if not FIRST <= index <= LAST:
        raise ValueError(f"Index {index} is outside the script table")
    address = struct.unpack_from('<I', rom, TABLE + 4 * index)[0]
    if not BASE <= address < BASE + len(rom):
        raise ValueError(f"Non-ROM pointer 0x{address:08X}")
    return address - BASE


def named_tokens(tokens):
    """Migrate already-repaired historical forms; never guess an argument."""
    result = []
    for t in tokens:
        if isinstance(t, list) and t[0] in ('BYTE', 'CONTROL'):
            code = t[1]
            name, nargs = OPS[code]
            if len(t) != 2 + nargs:
                raise ValueError(f'Missing argument in {t}')
            t = [Symbol(name), *t[2:]]
        result.append(t)
    return result


def decode_legacy(rom, index, big, small):
    """Compatibility representation used ONLY to repair historical broken dumps."""
    if index in CREDITS:
        raise ValueError("Credits have their own coordinate/text grammar")
    pos = pointer(rom, index)
    result = []

    def put(text):
        if result and type(result[-1]) is str:
            result[-1] += text
        else:
            result.append(text)

    for _ in range(16384):
        code = rom[pos]
        pos += 1
        if code == 0:
            return result
        if code == 1:
            code = 256 + rom[pos]
            pos += 1
            put(big[code])
        elif code >= 16:
            put(big[code])
        elif code == 5:
            name = ''
            for _ in range(256):
                value = rom[pos]
                pos += 1
                if value == 5:
                    break
                name += small[value]
            else:
                raise ValueError("Unterminated speaker name")
            result.append([Symbol('NAME'), name])
        elif code in (6, 10, 12, 13):
            arg = rom[pos]
            pos += 1
            result.append([Symbol('COLOR'), arg] if code == 12 else
                          [Symbol('CONTROL'), code, arg])
        elif code == 2:
            result.append([Symbol('NEWLINE')])
        elif code == 14:
            result.append([Symbol('PLAYER-NAME')])
        else:
            result.append([Symbol('BYTE'), code])
    raise ValueError("Unterminated dialogue")


def decode(rom, index, big, small):
    return named_tokens(decode_legacy(rom, index, big, small))


def legacy_tokens(tokens):
    result = []
    for t in tokens:
        if isinstance(t, list) and t[0] in BY_NAME and t[0] not in ('NEWLINE', 'COLOR', 'PLAYER-NAME'):
            code, nargs = BY_NAME[t[0]]
            if len(t) != 1 + nargs:
                raise ValueError(f'Malformed command: {t}')
            t = [Symbol('CONTROL' if nargs else 'BYTE'), code, *t[1:]]
        result.append(t)
    return result


def decode_credits(rom, index, big):
    """0807FC82: {tile-row, pixel-x or FF=center, text, 02}, ending in 00."""
    pos = pointer(rom, index)
    lines = []
    while rom[pos]:
        row, x = rom[pos:pos + 2]
        pos += 2
        start, text = pos, ''
        while rom[pos] not in (0, 2):
            code = rom[pos]
            pos += 1
            if code == 1:
                code = 256 + rom[pos]
                pos += 1
            text += big[code]
        lines.append({'tile_row': row, 'x': 'center' if x == 255 else x,
                      'encoded_bytes': pos - start, 'text': text})
        if rom[pos] == 0:
            break
        pos += 1
    return lines


def repair_arguments(tokens, original):
    """Recover argument bytes from the original, never from translated prose."""
    controls = [t for t in original if isinstance(t, list) and t[0] == 'CONTROL']
    result, edits, cursor = [], [], 0
    tokens = [t[:] if isinstance(t, list) else t for t in tokens]
    for n, token in enumerate(tokens):
        if token is None:
            continue
        if isinstance(token, list) and token[0] == 'CONTROL':
            if cursor >= len(controls) or token != controls[cursor]:
                raise ValueError("Structured control disagrees with original")
            cursor += 1
        if isinstance(token, list) and token[0] == 'BYTE' and token[1] in (6, 10, 13):
            if cursor >= len(controls) or controls[cursor][1] != token[1]:
                raise ValueError("Cannot align opcode arguments")
            fixed = controls[cursor]
            arg = fixed[2]
            cursor += 1
            if arg:
                following = tokens[n + 1] if n + 1 < len(tokens) else None
                if following == [Symbol('BYTE'), arg]:
                    tokens[n + 1] = None
                elif token[1] == 6 and arg == 64 and following == ' ':
                    # Entry 2019: the old reflow stripped argument glyph 40 (ぅ).
                    tokens[n + 1] = None
                else:
                    raise ValueError("Nonzero argument was edited as prose; manual repair required")
            token = fixed
            edits.append(f"Recovered {sexp(fixed)}")
        result.append(token)
    if cursor != len(controls):
        raise ValueError("Original controls are missing")
    return result, edits


def signature(tokens):
    return [t for t in tokens if isinstance(t, list) and
            (t[0] == 'CONTROL' or t[0] == 'BYTE' and t[1] not in (7, 8))]


def unknown_glyphs(tokens, big_path, small_path):
    def spellings(path):
        return sorted({line.split('=', 1)[1] for line in Path(path).read_text().splitlines()},
                      key=len, reverse=True)
    big, small = spellings(big_path), spellings(small_path)
    unknown = set()
    for token in tokens:
        if type(token) is str:
            text, choices = token, big
        elif isinstance(token, list) and token[0] == 'NAME':
            text, choices = token[1], small
        else:
            continue
        pos = 0
        while pos < len(text):
            found = next((g for g in choices if text.startswith(g, pos)), None)
            if found:
                pos += len(found)
            else:
                unknown.add(text[pos])
                pos += 1
    return sorted(unknown)


def audit(rom, entries, root):
    big_path, small_path = root / 'SlimeDialog.tbl', root / 'Slime_Small.tbl'
    big, small = table(big_path), table(small_path)
    report = {'rom_sha256': hashlib.sha256(rom).hexdigest(), 'entries': len(entries),
              'repairs': [], 'blocked': [], 'review': [], 'missing': [],
              'credits_original': {str(i): decode_credits(rom, i, big) for i in CREDITS}}
    repaired = []
    for index, *tokens in entries:
        try:
            original = decode_legacy(rom, index, big, small)
        except (ValueError, KeyError, IndexError) as exc:
            report['blocked'].append({'index': index, 'reason': str(exc)})
            continue
        original_controls = [t for t in original if isinstance(t, list) and t[0] == 'CONTROL']
        try:
            fixed, edits = repair_arguments(legacy_tokens(tokens), original)
            if signature(fixed) != signature(original):
                raise ValueError('Non-layout control sequence differs from original')
            if fixed and fixed[-1] == [Symbol('CONTROL'), 13, 0] and original_controls:
                raise ValueError('Text truncated at zero-valued counter argument; missing translated ending')
            unknown = unknown_glyphs(fixed, big_path, small_path)
            if unknown:
                raise ValueError('Unencodable glyphs: ' + repr(unknown))
            if index == 1881 and fixed[-2:] != [[Symbol('BYTE'), 7], [Symbol('BYTE'), 8]]:
                fixed += [[Symbol('BYTE'), 7], [Symbol('BYTE'), 8]]
                edits.append('Restored original terminal 07 08 after counter text')
            repaired.append([index, *named_tokens(fixed)])
            if edits:
                report['repairs'].append({'index': index, 'changes': edits})
        except ValueError as exc:
            report['blocked'].append({'index': index, 'reason': str(exc),
                                      'translation': sexp([index, *tokens]),
                                      'original': sexp([index, *named_tokens(original)])})
        old_names = original.count([Symbol('PLAYER-NAME')])
        new_names = tokens.count([Symbol('PLAYER-NAME')])
        if old_names != new_names:
            report['review'].append({'index': index, 'reason': 'Player-name occurrence count differs',
                                     'original': old_names, 'translation': new_names})
        if any(type(t) is str and re.search('[ぁ-ヿ一-龯]', t) for t in tokens):
            report['review'].append({'index': index, 'reason': 'Japanese remains in body (may be a proper name)'})
    present = {row[0] for row in entries}
    for index in range(FIRST, LAST + 1):
        if index in present:
            continue
        try:
            original = decode_legacy(rom, index, big, small)
        except ValueError as exc:
            if index in CREDITS:
                report['missing'].append({'index': index, 'reason': str(exc)})
            continue
        if original:
            report['missing'].append({'index': index, 'original': sexp([index, *named_tokens(original)])})
    report['preview_entries'] = len(repaired)
    report['release_ready'] = False  # Requires layout and in-game review as well.
    return report, repaired


def migrate_professional_dialogue(rom, entries, root):
    """Preserve prose (including known truncations) in a named review copy.

This is not the safe preview: trans still rejects truncated and unencodable
entries. Null slots and credits are excluded because they aren't dialogue.
"""
    big, small = table(root / 'SlimeDialog.tbl'), table(root / 'Slime_Small.tbl')
    result = []
    for index, *tokens in entries:
        if index in CREDITS:
            continue
        try:
            original = decode_legacy(rom, index, big, small)
        except ValueError:
            continue
        fixed, _ = repair_arguments(legacy_tokens(tokens), original)
        if signature(fixed) != signature(original):
            raise ValueError(f'Cannot safely migrate dialogue {index}')
        if index == 1881 and fixed[-2:] != [['BYTE', 7], ['BYTE', 8]]:
            fixed += [[Symbol('BYTE'), 7], [Symbol('BYTE'), 8]]
        result.append([index, *named_tokens(fixed)])
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', default='slime_original.gba')
    parser.add_argument('--script', default='text-dumps/professional-dialogue.txt')
    parser.add_argument('--report', default='text-dumps/translation-audit.json')
    parser.add_argument('--review-script', help='Optional named migration output; never overwrite the input')
    parser.add_argument('--preview-script', default='text-dumps/after-translate2-preview.txt')
    args = parser.parse_args()
    if args.review_script and Path(args.review_script).resolve() == Path(args.script).resolve():
        parser.error('--review-script must differ from --script')
    root = Path(__file__).resolve().parent.parent
    rom, entries = Path(args.rom).read_bytes(), read_script(args.script)
    report, repaired = audit(rom, entries, root)
    if args.review_script:
        named = migrate_professional_dialogue(rom, entries, root)
        Path(args.review_script).write_text(
            '; HUMAN DIALOGUE REVIEW COPY: named commands, original prose retained.\n'
            '; Known missing endings/unencodable glyphs remain blocked; see translation-audit.json.\n'
            '; Credits and null slots excluded. This is NOT the partial preview input.\n' +
            '\n'.join(map(sexp, named)) + '\n', encoding='utf-8')
    report['script'] = args.script
    Path(args.report).write_text(json.dumps(report, ensure_ascii=False, indent=2) + '\n')
    Path(args.preview_script).write_text(
        '; PARTIAL PREVIEW: omitted entries retain Japanese/original ROM data.\n'
        '; Generated by tools/audit_text.py; review translation-audit.json.\n' +
        '\n'.join(map(sexp, repaired)) + '\n', encoding='utf-8')
    print(f"{report['entries']} entries; {len(report['repairs'])} mechanically repaired; "
          f"{len(report['blocked'])} blocked; {len(report['missing'])} missing nonempty entries. "
          f"Partial preview input: {len(repaired)} entries.")


if __name__ == '__main__':
    main()
