#!/usr/bin/env python3
"""Build the current dialogue preview as IPS without opening or creating a ROM.

Only pointer replacements and newly encoded text enter the patch. The checked-in
profile supplies ROM identity/size and font widths; the existing original script
supplies slot validity and execution-control expectations. --verify-rom is an
optional LOCAL cross-check of that metadata against a user's original cartridge.
"""
import argparse
import hashlib
import json
import os
import struct
import subprocess
import tempfile
from pathlib import Path

from audit_text import read_script, sexp, Symbol
from check_professional import control_trace
from text_codec import Codec

ROOT = Path(__file__).resolve().parent.parent
IPS_LIMIT = 1 << 24
IPS_EOF_OFFSET = int.from_bytes(b'EOF', 'big')


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def ips_patch(writes):
    """Encode disjoint writes as standard literal IPS records (no size extension)."""
    merged = []
    for offset, data in sorted(writes):
        if not data or type(offset) is not int or offset < 0 or offset + len(data) > IPS_LIMIT:
            raise ValueError('IPS writes must be nonempty and within the first 16 MiB')
        if merged and offset < merged[-1][0] + len(merged[-1][1]):
            raise ValueError('Overlapping IPS writes')
        if merged and offset == merged[-1][0] + len(merged[-1][1]):
            merged[-1][1].extend(data)
        else:
            merged.append((offset, bytearray(data)))
    out = bytearray(b'PATCH')
    for offset, data in merged:
        for start in range(0, len(data), 65535):
            address = offset + start
            if address == IPS_EOF_OFFSET:
                raise ValueError('IPS record starts at reserved EOF offset 0x454F46')
            chunk = data[start:start + 65535]
            out.extend(address.to_bytes(3, 'big'))
            out.extend(len(chunk).to_bytes(2, 'big'))
            out.extend(chunk)
    out.extend(b'EOF')
    return bytes(out)


def canonical(tokens):
    """Expand the injector's two authoring shorthands before source comparison."""
    result = []
    for t in tokens:
        if t == ['WAIT-FOR-A']:
            result.extend([[Symbol('SHOW-PROMPT')], [Symbol('WAIT-INPUT')]])
        elif t == ['FORCE-NEWLINE']:
            result.append([Symbol('NEWLINE')])
        else:
            result.append(t)
    return result


def load_profile(root):
    path = root / 'tools/patch_profile.json'
    profile = json.loads(path.read_text())
    if profile['version'] != 1:
        raise ValueError('Unsupported patch profile version')
    if not 0 < profile['source_rom_size'] < IPS_LIMIT:
        raise ValueError('Invalid original ROM size')
    source_path = root / profile['original_dialogue_file']
    if sha256(source_path.read_bytes()) != profile['original_dialogue_sha256']:
        raise ValueError('Original dialogue inventory changed; verify/update the patch profile locally')
    records = profile['font_records']
    previous = 15
    for start, end, width in records:
        if start != previous + 1 or end < start or not 4 <= width <= 13:
            raise ValueError('Invalid font width ranges')
        previous = end
    if not records or previous != 0x1b8:
        raise ValueError('Incomplete font width ranges')
    source = {i: ts for i, *ts in read_script(source_path)}
    for i in source:
        pos = profile['pointer_table_offset'] + 4 * i
        if not 52 <= i <= 2397 or 1883 <= i <= 1901 or not 0 <= pos <= profile['source_rom_size'] - 4:
            raise ValueError(f'Invalid dialogue pointer slot {i}')
    return profile, source


def validate_entries(entries, source, codec):
    for i, *ts in entries:
        if i not in source:
            raise ValueError(f'{i}: not a valid original dialogue slot (null/credits/outside table)')
        tokens = canonical(ts)
        # Validation happens before reflow inserts any new page waits.
        if control_trace(tokens) != control_trace(source[i]):
            raise ValueError(f'{i}: execution controls differ from the original script')
        if tokens and tokens[-1] == ['DYNAMIC-TEXT', 0]:
            raise ValueError(f'{i}: truncated dynamic-counter ending')
        codec.encode(tokens)  # Unknown commands, arguments and glyphs fail the build.


def verify_profile(rom, profile, source, codec):
    if len(rom) != profile['source_rom_size'] or sha256(rom) != profile['source_rom_sha256']:
        raise ValueError('Wrong source ROM; use the unmodified ROM identified by patch_profile.json')
    for n, (first, _, width) in enumerate(profile['font_records']):
        if struct.unpack_from('<HB', rom, 0x713EB8 + n * 8 + 4) != (first, width):
            raise ValueError('Profile font metrics differ from the ROM')
    for i, tokens in source.items():
        offset = struct.unpack_from('<I', rom, profile['pointer_table_offset'] + 4 * i)[0] - profile['gba_base_address']
        encoded = codec.encode(tokens)
        if offset < 0 or rom[offset:offset + len(encoded)] != encoded:
            raise ValueError(f'{i}: original script/pointer metadata differs from ROM')


def build(script, output, root=ROOT, verify_rom=None):
    script, output = Path(script).resolve(), Path(output).resolve()
    profile, source = load_profile(root)
    codec = Codec(root)
    entries = read_script(script)
    validate_entries(entries, source, codec)
    if verify_rom is not None:
        verify_profile(Path(verify_rom).read_bytes(), profile, source, codec)
    with tempfile.TemporaryDirectory(prefix='translimeation-ips-') as tmp:
        temp = Path(tmp)
        metrics = temp / 'metrics.txt'
        metrics.write_text('\n'.join(map(sexp, profile['font_records'])) + '\n')
        subprocess.run(['sbcl', '--script', str(root / 'tools/build_patch_data.lisp'),
                        str(script), str(metrics), str(temp)], cwd=root, check=True)
        payload = (temp / 'payload.bin').read_bytes()
        records = read_script(temp / 'records.txt')
        held = read_script(temp / 'held.txt')
        flowed = read_script(temp / 'reflowed.txt')
    if not records:
        raise ValueError('No injectable entries; refusing to produce an empty preview')
    if [r[0] for r in records] != [r[0] for r in flowed]:
        raise ValueError('Encoder/reflow index mismatch')
    if {r[0] for r in records} & {r[0] for r in held} or sorted(r[0] for r in records + held) != sorted(r[0] for r in entries):
        raise ValueError('Encoder lost, duplicated or invented a script entry')
    base = profile['source_rom_size']
    if base + len(payload) > IPS_LIMIT:
        raise ValueError('Appended text exceeds standard IPS 16 MiB address space; use another format')
    writes, cursor = [], 0
    for (i, offset, size), row in zip(records, flowed):
        if offset != cursor or size < 1 or offset + size > len(payload):
            raise ValueError(f'{i}: invalid appended text span')
        # Independent Python encoder must agree with the shared Lisp encoder.
        if codec.encode(row[1:]) != payload[offset:offset + size]:
            raise ValueError(f'{i}: Lisp/Python encoders disagree')
        pointer = profile['gba_base_address'] + base + offset
        writes.append((profile['pointer_table_offset'] + 4 * i, struct.pack('<I', pointer)))
        cursor += size
    if cursor != len(payload):
        raise ValueError('Unaccounted bytes in appended text')
    writes.append((base, payload))
    patch = ips_patch(writes)
    sources = ['slurp.lisp', 'SlimeDialog.tbl', 'Slime_Small.tbl', 'tools/patch_profile.json',
               profile['original_dialogue_file'], 'tools/build_patch_data.lisp',
               'tools/build_ips.py', 'tools/audit_text.py', 'tools/check_professional.py', 'tools/text_codec.py']
    inputs = {p: sha256((root / p).read_bytes()) for p in sources}
    try:
        script_label = script.relative_to(root).as_posix()
    except ValueError:
        script_label = script.name
    inputs[script_label] = sha256(script.read_bytes())
    revision = os.environ.get('GITHUB_SHA')
    if not revision:
        try:
            revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root,
                                               stderr=subprocess.DEVNULL, text=True).strip()
        except (OSError, subprocess.CalledProcessError):
            revision = None
    report = {'format': 'IPS', 'partial_preview': True, 'script': script_label,
              'source_commit': revision, 'input_sha256': inputs,
              'source_rom_size': base, 'source_rom_sha256': profile['source_rom_sha256'],
              'patched_rom_size': base + len(payload), 'patch_sha256': sha256(patch),
              'patch_bytes': len(patch), 'appended_bytes': len(payload),
              'injected_entries': [r[0] for r in records],
              'held_entries': [{'index': i, 'reason': reason} for i, reason in held],
              'credits_included': False}
    output.mkdir(parents=True, exist_ok=True)
    for name, data in [('slime-professional-preview.ips', patch),
                       ('build-report.json', (json.dumps(report, indent=2) + '\n').encode())]:
        with tempfile.NamedTemporaryFile(dir=output, delete=False) as f:
            f.write(data)
            staged = Path(f.name)
        staged.replace(output / name)
    print(f'{len(records)} entries; {len(held)} layout holds; {len(patch):,}-byte IPS: '
          f'{output / "slime-professional-preview.ips"}')
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--script', type=Path, default=ROOT / 'text-dumps/professional-dialogue.txt')
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'dist')
    parser.add_argument('--verify-rom', type=Path, help='Optional local metadata verification; never uploaded or copied')
    args = parser.parse_args()
    try:
        build(args.script, args.output_dir, verify_rom=args.verify_rom)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        parser.exit(1, f'IPS build failed: {exc}\n')


if __name__ == '__main__':
    main()
