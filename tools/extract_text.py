#!/usr/bin/env python3
"""Reproducible, lossless census of the original ROM's encoded text.

Addresses/bounds come from ROM consumers, not a scan for plausible Japanese.
Every byte in the main text section and the separate resident-name section is
accounted for. Unreferenced scripts are retained without claiming reachability.
No ROM is modified. Original historical translation dumps are never overwritten.
"""
import argparse
from collections import Counter, defaultdict
import hashlib
import json
from pathlib import Path
import struct

from text_codec import Codec, OPS, Symbol, sexp, read_script

BASE = 0x08000000
SHA256 = 'f86a933440369e13a6898864d1ac10b8af409674c489a2ccc9a89cdfa6d2a661'
BANKS, BANKS_END = 0x713CC4, 0x713EB8
LEGACY = 0x71174C
POOL, POOL_END = 0x713F08, 0x73833C
NAMES, NAMES_END = 0x7D1428, 0x7D17BE
RENDERERS = {0x95E18: 'dialogue-loader', 0x96684: 'fast-dialogue-loader',
             0x96580: 'speaker-name', 0x969B8: 'dialogue-glyph',
             0x96AA8: 'variable-width-glyph', 0x96BC8: 'centered-glyph',
             0x96C40: 'plain-text', 0x96CDC: 'glyph-blitter',
             0x96FE8: 'small-text', 0x970D0: 'plain-text-wrapper',
             0x9670C: 'dynamic-number-formatter', 0xD3468: 'name-grid-decoder',
             0xD3600: 'name-entry-ticker'}


def u32(rom, pos):
    return struct.unpack_from('<I', rom, pos)[0]


def symbolic(tokens):
    return [[Symbol(t[0]), *t[1:]] if isinstance(t, list) else t for t in tokens]


def scan_references(rom):
    refs, literals = defaultdict(list), defaultdict(list)
    # Include unaligned matches as candidates, NEVER as automatic patch sites.
    for pos in range(len(rom) - 3):
        if rom[pos+3] != 8:
            continue
        ptr = u32(rom, pos) - BASE
        if POOL <= ptr < POOL_END or NAMES <= ptr < NAMES_END:
            refs[ptr].append(pos)
    for pos in range(0, 0xE0000, 2):
        ins = struct.unpack_from('<H', rom, pos)[0]
        if ins & 0xF800 == 0x4800:  # Thumb LDR Rd,[PC,#imm]
            slot = ((pos + 4) & ~3) + ((ins & 255) * 4)
            literals[slot].append(pos)
    return refs, literals


def call_census(rom):
    calls = {f'{BASE+p:08X}': {'name': n, 'thumb_bl_sites': [], 'pointer_candidates': []}
             for p, n in RENDERERS.items()}
    for pos in range(0, len(rom) - 3, 2):
        a, b = struct.unpack_from('<HH', rom, pos)
        if a & 0xF800 != 0xF000 or b & 0xF800 != 0xF800:
            continue
        delta = ((a & 2047) << 12) | ((b & 2047) << 1)
        if delta & 0x400000:
            delta -= 0x800000
        target = pos + 4 + delta
        if target in RENDERERS:
            calls[f'{BASE+target:08X}']['thumb_bl_sites'].append(f'{BASE+pos:08X}')
    for target in RENDERERS:
        for mode in (0, 1):
            needle, start = struct.pack('<I', BASE + target + mode), 0
            while (start := rom.find(needle, start)) >= 0:
                calls[f'{BASE+target:08X}']['pointer_candidates'].append(f'{start:06X}')
                start += 1
    return calls


def extract(rom, root):
    if hashlib.sha256(rom).hexdigest() != SHA256:
        raise ValueError('This census is verified for slime_original.gba only (SHA-256 mismatch)')
    codec = Codec(root)
    refs, literals = scan_references(rom)
    records, by_key, covered, nulls = [], {}, bytearray(len(rom)), []
    bank_starts = [u32(rom, p) - BASE for p in range(BANKS, BANKS_END, 4)]
    start = min(bank_starts)
    assert start == 0x71181C and all(start <= p < BANKS and p % 4 == 0 for p in bank_starts)
    boundaries = sorted(set(bank_starts) | {BANKS})
    banks = [{'bank': i, 'table_offset': f'{p:06X}', 'first_legacy_index': (p-LEGACY)//4,
              'slots_until_next_distinct_start': (boundaries[boundaries.index(p)+1]-p)//4}
             for i, p in enumerate(bank_starts)]
    credit_refs = {u32(rom, p)-BASE: p for p in range(0x557A30, 0x557A80, 4) if u32(rom, p)}

    def add(pos, kind, evidence, legacy=None, count=None):
        key = (pos, kind)
        if key in by_key:
            record = by_key[key]
            if legacy is not None:
                record['legacy_indices'].append(legacy)
            if evidence not in record['evidence']:
                record['evidence'].append(evidence)
            return record
        if kind == 'credits':
            tokens, end = codec.credits(rom, pos)
            encoded = codec.encode_credits(tokens)
        elif kind == 'small':
            end = rom.index(0, pos) + 1
            tokens = [''.join(codec.small[c] for c in rom[pos:end-1])]
            encoded = codec.encode_text(tokens[0], True) + b'\0'
        elif kind in ('name-grid', 'digit-table'):
            tokens, end = [], pos
            for _ in range(count):
                c = rom[end]; end += 1
                if c == 1:
                    c = 256 + rom[end]; end += 1
                if c in (14, 15):
                    tokens.append(['DAKUTEN' if c == 14 else 'HANDAKUTEN'])
                else:
                    codec.glyph(tokens, c)
            encoded = bytearray()
            for t in tokens:
                if t == ['DAKUTEN']: encoded.append(14)
                elif t == ['HANDAKUTEN']: encoded.append(15)
                else: encoded.extend(codec.encode([t], 'plain')[:-1])
            encoded = bytes(encoded)
        else:
            tokens, end = codec.decode(rom, pos, kind)
            encoded = codec.encode(tokens, kind)
        if encoded != rom[pos:end]:
            raise ValueError(f'Round-trip mismatch at {pos:06X}: {kind}')
        rec = {'id': f'{kind}-{pos:06X}', 'offset': f'{pos:06X}', 'end': f'{end:06X}',
               'format': kind, 'legacy_indices': [] if legacy is None else [legacy],
               'evidence': [evidence], 'tokens': tokens, 'encoded_bytes': end-pos,
               'original_hex': rom[pos:end].hex(), 'references': []}
        for ref in refs[pos]:
            reason = ('dialogue-table' if start <= ref < BANKS and ref % 4 == 0 else
                      'credits-table' if credit_refs.get(pos) == ref else
                      'item-label-table' if 0x1C9BC4 <= ref < 0x1C9CB4 and ref % 4 == 0 else
                      'resident-name-table' if 0x7654E4 <= ref < 0x765674 and ref % 4 == 0 else
                      'window-descriptor' if ref in (0x73840C, 0x738490, 0x7384A8, 0x7384C0, 0x7384F0, 0x73853C) else
                      'code-literal-candidate' if ref in literals else 'unclassified-pointer-pattern')
            rec['references'].append({'offset': f'{ref:06X}', 'kind': reason})
        records.append(rec); by_key[key] = rec
        covered[pos:end] = b'\1' * (end-pos)
        return rec

    for slot in range(start, BANKS, 4):
        pos = u32(rom, slot) - BASE
        index = (slot - LEGACY) // 4
        if pos == -BASE:
            nulls.append(index); continue
        if not POOL <= pos < POOL_END:
            raise ValueError(f'Unexpected table pointer at {slot:06X}')
        kind = 'credits' if pos in credit_refs else 'dialogue'
        add(pos, kind, 'bank-directory', index)
    assert len(credit_refs) == 19
    for pos in credit_refs:
        add(pos, 'credits', 'independent-credits-table')

    # Independent strings/tables before credits. Boundaries and lengths are
    # established by their consumers: 0967xx, 097xxx, 0C5xxx, 0D1xxx and 0840xx.
    pos = POOL
    while pos < 0x713F7E:
        rec = add(pos, 'plain', 'menu/record-window-consumers')
        pos = int(rec['end'], 16)
    add(0x713F7E, 'digit-table', '0809670C and record-number builders', count=10)
    for pos in (0x713F88, 0x713F8E):
        add(pos, 'plain', 'save-slot-window-consumers')
    add(0x713F9E, 'name-grid', '080D1FB8: 180 cells; 080D2E64: kana modifiers', count=180)
    add(0x714054, 'name-grid', '080D1FC8: 50 voiced/semi-voiced kana cells', count=50)
    pos = 0x714091
    while pos < 0x7140C1:
        rec = add(pos, 'plain', 'default-name / name-validation consumers 080D31E8..080D33EC')
        pos = int(rec['end'], 16)
    add(0x7140C1, 'plain', '0808401C: big-to-small player-name glyph conversion lookup')

    # The shop UI uses another pointer table at 081C9BC4 and direct literals.
    pos = 0x72A204
    while pos < 0x72A454 and rom[pos]:
        rec = add(pos, 'plain', 'shop/item text block')
        pos = int(rec['end'], 16)
    add(0x72A3EA, 'plain', 'shared blank suffix; direct literals and item-table aliases')

    # Separate 100-resident small-font names: found through 08096FE8 callers.
    for slot in range(0x7654E4, 0x765674, 4):
        add(u32(rom, slot)-BASE, 'small', 'resident-list renderer 080CB686 and related callers')
    add(0x7D17AC, 'small', 'resident-list unknown-name placeholder')
    add(0x7D17B5, 'small', 'adjacent blank resident-list placeholder')

    # Decode gaps including prefixes whose pointers enter after the speaker name
    # or initial space. These records may overlap a table entry; retain both.
    padding = []
    pos = POOL
    while pos < POOL_END:
        if covered[pos]: pos += 1; continue
        if rom[pos] == 0:
            begin = pos
            while pos < POOL_END and not covered[pos] and rom[pos] == 0: pos += 1
            padding.append({'offset': f'{begin:06X}', 'bytes': pos-begin})
            covered[begin:pos] = b'\1' * (pos-begin)
        else:
            rec = add(pos, 'dialogue', 'text-section gap; no direct reference established')
            pos = int(rec['end'], 16)
    assert all(covered[POOL:POOL_END]) and all(covered[NAMES:NAMES_END])
    records.sort(key=lambda r: (r['offset'], r['format']))
    # Any remaining pointer-pattern targets are interior aliases, not discarded
    # strings. Record their enclosing data for inspection (many are coincidences).
    interior = []
    starts = {int(rec['offset'], 16) for rec in records}
    for target, sites in sorted(refs.items()):
        if target in starts: continue
        owners = [rec['id'] for rec in records if int(rec['offset'], 16) < target < int(rec['end'], 16)]
        interior.append({'target': f'{target:06X}', 'containers': owners,
                         'reference_patterns': [f'{p:06X}' for p in sites]})
    old = {r[0] for r in read_script(root/'text-dumps/before-translate2.txt')}
    missing = [i for r in records if r['tokens'] for i in r['legacy_indices'] if i not in old]
    opcode_counts = Counter(t[0] for rec in records if rec['format'] == 'dialogue'
                            for t in rec['tokens'] if isinstance(t, list))
    report = {'rom_sha256': SHA256, 'bank_directory': banks, 'null_slots': nulls,
              'legacy_slots': (BANKS-start)//4, 'unique_records': len(records),
              'formats': dict(Counter(r['format'] for r in records)),
              'extra_records_without_legacy_index': sum(not r['legacy_indices'] for r in records),
              'missing_nonempty_legacy_entries': sorted(missing),
              'round_trip_records': len(records),
              'covered_sections': [{'start': f'{POOL:06X}', 'end': f'{POOL_END:06X}', 'bytes': POOL_END-POOL, 'unclassified_bytes': 0},
                                   {'start': f'{NAMES:06X}', 'end': f'{NAMES_END:06X}', 'bytes': NAMES_END-NAMES, 'unclassified_bytes': 0}],
              'zero_padding': padding, 'interior_pointer_patterns': interior,
              'renderer_call_census': call_census(rom),
              'dialogue_opcodes': [{'hex': f'{code:02X}', 'name': name, 'argument_bytes': nargs,
                                    'occurrences_in_dialogue_views': opcode_counts[name]}
                                   for code, (name, nargs) in OPS.items()],
              'hardcoded_glyphs': [{'glyph': 0x111, 'text': codec.big[0x111], 'literal_offset': '0984EC', 'consumer': '08098474', 'purpose': 'save-slot resident count suffix'},
                                  {'glyph': 0x16, 'text': codec.big[0x16], 'instruction_offsets': ['0984A4', '0984CC'], 'purpose': 'save-slot time separators'}],
              'scope': ['Complete byte coverage of both identified encoded-text sections, including unreferenced scripts.',
                        'Whole-ROM direct Thumb BL and pointer-pattern census for identified text renderers.',
                        'No claim that unreferenced content is reachable in gameplay.',
                        'Graphics/tilemaps can contain Japanese without encoded strings; assets require a separate visual localization audit.',
                        'Pointer-pattern matches are evidence for review, not automatic relocation authorization.']}
    return records, report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', default='slime_original.gba')
    parser.add_argument('--output-dir', default='text-dumps')
    args = parser.parse_args()
    root = Path(__file__).resolve().parent.parent
    records, report = extract(Path(args.rom).read_bytes(), root)
    out = Path(args.output_dir); out.mkdir(parents=True, exist_ok=True)
    for name, value in [('rom-text-inventory.json', records), ('rom-text-coverage.json', report)]:
        (out/name).write_text(json.dumps(value, ensure_ascii=False, indent=2)+'\n')
    indexed, extras = [], ['; Additional records; addresses are ROM file offsets. See JSON for format and references.']
    for r in records:
        if r['format'] == 'dialogue' and r['legacy_indices']:
            indexed.extend([i, *symbolic(r['tokens'])] for i in r['legacy_indices'])
        elif not r['legacy_indices']:
            extras.append(f"; {r['id']}: {'; '.join(r['evidence'])}")
            extras.append(sexp([int(r['offset'], 16), *symbolic(r['tokens'])]))
    (out/'rom-dialogue.txt').write_text('; Complete original dialogue, including empty slots; credits are in the JSON inventory.\n' +
                                      '\n'.join(sexp(r) for r in sorted(indexed))+'\n')
    (out/'rom-extra-text.txt').write_text('\n'.join(extras)+'\n')
    print(f"{len(records)} records round-trip byte-for-byte; {report['extra_records_without_legacy_index']} outside the legacy table; "
          f"{len(report['bank_directory'])} banks; zero unclassified bytes in both text sections.")


if __name__ == '__main__':
    main()
