#!/usr/bin/env python3
"""Build the current dialogue preview as IPS without opening or creating a ROM.

The patch contains dialogue pointers/text and the Rocket Slime font/selector.
The checked-in profile supplies ROM identity/size and valid pointer slots. --verify-rom is an
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

from text_codec import Codec, read_script, read_dialogue, sexp, validate_dialogue
from rocket_font import UI_CODE, UI_SOURCE, ASSET, load_font, font_metrics, pack_font, verify_font_source
from text_layout import (LAYOUTS, READING_CODE, READING_SOURCE, load_layouts,
                         validate_layouts, lisp_layouts, pack_reading_pause, verify_reading_hooks)

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


def load_profile(root):
    path = root / 'tools/patch_profile.json'
    profile = json.loads(path.read_text())
    if profile['version'] != 2:
        raise ValueError('Unsupported patch profile version')
    if not 0 < profile['source_rom_size'] < IPS_LIMIT:
        raise ValueError('Invalid original ROM size')
    records = profile['font_records']
    previous = 15
    for start, end, width in records:
        if start != previous + 1 or end < start or not 4 <= width <= 13:
            raise ValueError('Invalid font width ranges')
        previous = end
    if not records or previous != 0x1b8:
        raise ValueError('Incomplete font width ranges')
    slots = set()
    previous = 51
    for start, end in profile['dialogue_slot_ranges']:
        if not previous < start <= end <= 2397:
            raise ValueError('Invalid dialogue slot ranges')
        slots.update(range(start, end + 1))
        previous = end
    if not slots:
        raise ValueError('Empty dialogue slot inventory')
    for i in slots:
        pos = profile['pointer_table_offset'] + 4 * i
        if not 52 <= i <= 2397 or 1883 <= i <= 1901 or not 0 <= pos <= profile['source_rom_size'] - 4:
            raise ValueError(f'Invalid dialogue pointer slot {i}')
    return profile, slots


def validate_entries(entries, slots, codec):
    for i, *ts in entries:
        if i not in slots:
            raise ValueError(f'{i}: not a valid original dialogue slot (null/credits/outside table)')
        try:
            validate_dialogue(ts, codec)
        except ValueError as exc:
            raise ValueError(f'{i}: {exc}') from exc


def pack_menu_text(rows, profile, codec, offset):
    """Relocate UI text, padding its fields to the verified tile allocations.

    The shared plain renderer selects DS metrics for expanded-ROM strings.
    Original/RAM text retains stock metrics. Byte lookup tables are not rendered.
    """
    entries = {r[0]: r for r in rows}
    widths = {c: width + 1 for first, last, width in profile['font_records']
              for c in range(first, last + 1)}
    widths.update({c: width for c, (width, _) in load_font().items()})
    writes, payload, included = [], bytearray(), []
    for key, layout in profile.get('plain_text', {}).items():
        index = int(key, 16)
        entry = entries.get(index)
        if entry is None:
            continue
        if len(entry) != 2 or not isinstance(entry[1], list) or entry[1][0] != 'PLAIN':
            raise ValueError(f'{index}: expected a PLAIN UI record')
        tokens, options = entry[1][1:], [[]]
        for token in tokens:
            if token == ['ALIGN']:
                options.append([])
            else:
                options[-1].append(token)
        encoded = bytearray()
        if 'glyphs' in layout:
            encoded = bytearray(codec.encode(tokens, 'plain'))
            # These consumers read one encoded byte, or the two-byte 匹 glyph.
            if layout['glyphs'] == 10:
                if len(encoded) != 11 or any(c < 16 for c in encoded[:-1]):
                    raise ValueError(f'{index}: digit lookup must contain ten single-byte glyphs')
            elif len(encoded) > 3 or (len(encoded) == 3 and encoded[0] != 1):
                raise ValueError(f'{index}: counter suffix must be empty or one glyph')
            encoded.extend(bytes(max(0, 3 - len(encoded))))
        else:
            if len(options) != len(layout['columns']):
                raise ValueError(f"{index}: preserve {len(layout['columns'])} fields separated by ALIGN")
            for option, columns in zip(options, layout['columns']):
                data = iter(codec.encode(option, 'plain')[:-1])
                glyphs = [256 + next(data) if c == 1 else c for c in data]
                while glyphs and glyphs[-1] == 0x1D:
                    glyphs.pop()
                # Leading blanks reserve room for fixed-position runtime numbers.
                leading = 0
                while leading < len(glyphs) and glyphs[leading] == 0x1D:
                    leading += 1
                glyphs[:leading] = [0x1D] * ((leading * 7 + widths[0x1D] - 1) // widths[0x1D])
                pixels = sum(widths[c] for c in glyphs)
                if pixels > columns * 8:
                    raise ValueError(f'{index}: UI field needs {pixels} pixels; available {columns * 8}')
                while (pixels + 7) // 8 < columns:
                    glyphs.append(0x1D)
                    pixels += widths[0x1D]
                for c in glyphs:
                    encoded.extend([1, c - 256] if c >= 256 else [c])
                encoded.append(2)  # Flush the last partial tile as well.
            encoded.append(0)
        pointer = profile['gba_base_address'] + offset + len(payload)
        writes.extend((site, struct.pack('<I', pointer)) for site in layout['pointers'])
        payload.extend(encoded)
        included.append(index)
    return writes, bytes(payload), included


def verify_profile(rom, profile, slots):
    if len(rom) != profile['source_rom_size'] or sha256(rom) != profile['source_rom_sha256']:
        raise ValueError('Wrong source ROM; use the unmodified ROM identified by patch_profile.json')
    verify_font_source(rom, profile)
    verify_reading_hooks(rom)
    for n, (first, _, width) in enumerate(profile['font_records']):
        if struct.unpack_from('<HB', rom, 0x713EB8 + n * 8 + 4) != (first, width):
            raise ValueError('Profile font metrics differ from the ROM')
    actual_slots = set()
    for i in range(52, 2398):
        if 1883 <= i <= 1901:
            continue
        offset = struct.unpack_from('<I', rom, profile['pointer_table_offset'] + 4 * i)[0] - profile['gba_base_address']
        if 0 <= offset < len(rom):
            actual_slots.add(i)
    if slots != actual_slots:
        raise ValueError('Profile dialogue slots differ from the ROM')


def build(script, output, root=ROOT, verify_rom=None, output_rom=None, automatic_pages=False):
    script, output = Path(script).resolve(), Path(output).resolve()
    profile, slots = load_profile(root)
    codec = Codec(root)
    entries = read_dialogue(script)
    validate_entries(entries, slots, codec)
    layouts = load_layouts(root)
    validate_layouts(entries, layouts, manual_breaks=not automatic_pages)
    if output_rom is not None and verify_rom is None:
        raise ValueError('--output-rom requires --verify-rom')
    if output_rom is not None and Path(output_rom).resolve() == Path(verify_rom).resolve():
        raise ValueError('Refusing to overwrite the original ROM')
    original = None
    if verify_rom is not None:
        original = Path(verify_rom).read_bytes()
        verify_profile(original, profile, slots)
    glyphs = load_font(root)
    with tempfile.TemporaryDirectory(prefix='translimeation-ips-') as tmp:
        temp = Path(tmp)
        metrics = temp / 'metrics.txt'
        metrics.write_text('\n'.join(map(sexp, font_metrics(profile, glyphs))) + '\n')
        policies = temp / 'layouts.txt'
        policies.write_text('\n'.join(map(sexp, lisp_layouts(layouts))) + '\n')
        subprocess.run(['sbcl', '--script', str(root / 'tools/build_patch_data.lisp'),
                        str(script), str(metrics), str(policies), str(temp),
                        "automatic" if automatic_pages else "manual"], cwd=root, check=True)
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
    text_bytes = len(payload)
    menu_offset = base + len(payload)
    menu_writes, menu, menu_entries = pack_menu_text(read_script(script), profile, codec, menu_offset)
    writes.extend(menu_writes)
    payload += menu
    payload += bytes((-len(payload)) & 3)
    font_offset = base + len(payload)
    font_writes, font = pack_font(font_offset, profile, glyphs)
    payload += font
    payload += bytes((-len(payload)) & 3)
    reading_offset = base + len(payload)
    reading_writes, reading_code = pack_reading_pause(reading_offset, root)
    payload += reading_code
    writes.extend(font_writes + [(base, payload)])
    writes.extend(reading_writes)
    patch = ips_patch(writes)
    sources = ['slurp.lisp', 'SlimeDialog.tbl', 'Slime_Small.tbl', 'tools/patch_profile.json',
               'tools/build_patch_data.lisp',
               'tools/build_ips.py', 'tools/text_codec.py', 'tools/rocket_font.py', ASSET, UI_CODE, UI_SOURCE,
               'tools/text_layout.py', LAYOUTS, READING_CODE, READING_SOURCE]
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
              'text_bytes': text_bytes, 'font_offset': font_offset, 'font_bytes': len(font),
              'menu_offset': menu_offset, 'menu_bytes': len(menu),
              'injected_plain_entries': menu_entries,
              'unreferenced_plain_entries': [i for i in menu_entries
                                            if not profile['plain_text'][f'{i:06x}']['pointers']],
              'reading_pause_offset': reading_offset,
              'injected_entries': [r[0] for r in records],
              'held_entries': [{'index': i, 'reason': reason} for i, reason in held],
              'credits_included': False, 'automatic_pages': automatic_pages}
    output.mkdir(parents=True, exist_ok=True)
    for name, data in [('slime-patch.ips', patch),
                       ('build-report.json', (json.dumps(report, indent=2) + '\n').encode())]:
        with tempfile.NamedTemporaryFile(dir=output, delete=False) as f:
            f.write(data)
            staged = Path(f.name)
        staged.replace(output / name)
    if output_rom is not None:
        result = bytearray(original)
        result.extend(bytes(base + len(payload) - len(result)))
        for offset, data in writes:
            result[offset:offset + len(data)] = data
        destination = Path(output_rom)
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(result)
    print(f'{len(records)} dialogue entries; {len(menu_entries)} UI records; '
          f'{len(held)} layout holds; {len(patch):,}-byte IPS: '
          f'{output / "slime-patch.ips"}')
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--script', type=Path, default=ROOT / 'text-dumps/gerb.txt')
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'dist')
    parser.add_argument('--verify-rom', type=Path, help='Optional local metadata verification; never uploaded or copied')
    parser.add_argument('--output-rom', type=Path, help='Also create a local ROM using --verify-rom as the source')
    parser.add_argument('--automatic-pages', action='store_true',
                        help='Use automatic reading pages with a CUE-based script')
    args = parser.parse_args()
    try:
        build(args.script, args.output_dir, verify_rom=args.verify_rom, output_rom=args.output_rom,
              automatic_pages=args.automatic_pages)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        parser.exit(1, f'IPS build failed: {exc}\n')


if __name__ == '__main__':
    main()
