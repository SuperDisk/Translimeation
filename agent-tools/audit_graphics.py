#!/usr/bin/env python3
"""Extract ROM resources and reconstruct graphics from their loader tables.

--full includes world scenes, sprite cells, UI layers and the pot title animation.
Whole-ROM stream candidates are retained separately from verified associations.
"""
import _paths

import argparse
import collections
import csv
import html
import struct
from pathlib import Path

from slime_gfx import Decompressor, DecodeError, gba_bgr555_to_rgba

# References to these roots feed the indexed getter at 08000858.
ARCHIVES = (0x1D9FEC, 0x44D550, 0x484474, 0x54B4FC, 0x765FA8)


def discover(rom):
    """Directory entries are exact; whole-ROM stream candidates remain provisional."""
    refs = collections.defaultdict(list)
    for p in range(0, len(rom) - 3, 4):
        value = struct.unpack_from('<I', rom, p)[0] - 0x08000000
        if 0 <= value < len(rom):
            refs[value].append(p)
    nodes = {}

    def add(p, size, key, evidence):
        if p in nodes:
            nodes[p]['keys'].append(key)
            return
        data = rom[p:p + size]
        node = dict(offset=p, size=size, keys=[key], evidence=evidence,
                    refs=refs[p], kind='raw', raw=data, decoded=None)
        nodes[p] = node
        if data[:1] == b'\x70':
            try:
                decoded, _, end = Decompressor(data, 0, max_output=0x40000).decompress()
                node.update(decoded=decoded, kind='stream')
            except (DecodeError, ValueError, IndexError):
                pass
        if len(data) < 12:
            return
        count = struct.unpack_from('<I', data)[0]
        base = 4 + count * 8
        if not 0 < count < 1000 or base >= len(data):
            return
        entries = list(struct.iter_unpack('<II', data[4:base]))
        # Nested directories use the same format; reject partial/random matches.
        if (entries[0][0] != 0 or any(o + n + base > len(data) for o, n in entries)
                or not len(data) - 3 <= base + max(o + n for o, n in entries) <= len(data)):
            return
        node['kind'] = 'archive'
        for i, (o, n) in enumerate(entries):
            add(p + base + o, n, f'{key}/{i:03x}', 'nested-directory')

    for root in ARCHIVES:
        for i, p, size in archive_entries(rom, root):
            add(p, size, f'{root:06x}/{i:03x}', 'directory')
    for p in range(0, len(rom) - 8, 4):
        if rom[p] != 0x70 or p in nodes:
            continue
        size = struct.unpack_from('<I', rom, p)[0] >> 8
        if not 2 <= size <= 0x40000:
            continue
        try:
            _, _, end = Decompressor(rom, p, max_output=0x40000).decompress()
        except (DecodeError, ValueError, IndexError):
            continue
        if end - p <= 0x40000:
            add(p, end - p, f'stream/{p:06x}',
                'pointer-referenced-stream' if refs[p] else 'scan-only')
    return nodes


def export_resources(nodes, output):
    for sub in ('raw', 'decoded'):
        (output / sub).mkdir(parents=True, exist_ok=True)
    with (output / 'inventory.csv').open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(('offset', 'size', 'keys', 'evidence', 'decoded_size', 'kind', 'refs'))
        for p, node in sorted(nodes.items()):
            (output / 'raw' / f'{p:06x}.bin').write_bytes(node['raw'])
            if node['decoded'] is not None:
                (output / 'decoded' / f'{p:06x}.bin').write_bytes(node['decoded'])
            writer.writerow((f'{p:06x}', node['size'], ';'.join(node['keys']), node['evidence'],
                             len(node['decoded']) if node['decoded'] is not None else '',
                             node['kind'], ';'.join(f'{v:06x}' for v in node['refs'])))


def gallery(output):
    """Build an offline gallery of reconstructed images, not diagnostic tile banks.

    Optional text-review.csv contains local human review: target,status,note.
    Targets are preview paths or tile:<hex offset>; statuses confirmed/possible/none.
    Missing reviews remain explicitly unreviewed, never inferred negative.
    """
    def read_csv(path):
        if not path.exists():
            return []
        with path.open() as stream:
            return list(csv.DictReader(stream))

    review = {r['target']: r for r in read_csv(output / 'text-review.csv')}
    cards = []
    def add(name, paths, group, offsets='', note='', known=True, evidence=''):
        if not paths or not (output / paths[0]).exists():
            return
        findings = [review[p] for p in paths if p in review]
        findings += [review['tile:' + p] for p in offsets.split(';')
                     if 'tile:' + p in review]
        # World scenes must be reviewed as assembled maps. Shared tilesets can
        # contain lettering that a particular map never actually uses.
        if group == 'World':
            findings = [review[p] for p in paths if p in review]
        status = next((s for s in ('confirmed', 'possible', 'none')
                       if any(f['status'] == s for f in findings)), 'unreviewed')
        notes = '; '.join(dict.fromkeys(f['note'] for f in findings))
        cards.append(dict(name=name, paths=paths, group=group, offsets=offsets,
                          note='; '.join(filter(None, (note, notes))), status=status,
                          known=known, evidence=evidence))

    add('Pot title and entrance animation', ['pot-title/pot-title.gif', 'pot-title/pot-title.png'],
        'UI', '1f6d98;7c7ab4',
        'Original sprite timing and changing background tilemap; BG palette matches companion asset and screenshot, live palette loader untraced',
        evidence='080c5980; 080c5ba6; 080d1b8c; 08002140; 0800188c')
    add('Title screen logo cells', ['known/title-sprites.png'], 'UI', '7c5624',
        evidence='080d4484..080d44ba')
    for row in read_csv(output / 'backgrounds/backgrounds.csv'):
        add(row['name'], ['backgrounds/' + row['preview']],
            'World' if row['name'].startswith('world-') else 'Background', row['tiles'],
            row.get('note', ''), evidence=row['evidence'])
    for row in read_csv(output / 'ui/ui.csv'):
        add(row['name'], ['ui/' + row['preview']], 'UI', row['tiles'], row['note'],
            evidence=row['evidence'])
    sprites = read_csv(output / 'sprites/sprites.csv')
    for row in sprites:
        paths = ['sprites/' + p for p in row['sheets'].split(';') if p]
        known = row['palette_evidence'].startswith('verified') and not row['warnings']
        add(row['archive'] + '/' + row['tiles_id'] + ' sprites', paths, 'Sprites',
            row['tiles'], row['palette_evidence'] + '; ' + row['warnings'], known,
            row['evidence'] + '; cells ' + row['cells'] + '; palette ' + row['palette'])

    # Keep extraction gaps visible even when no usable image can be made.
    unresolved = []
    paired = {r['cells'] for r in sprites}
    for row in read_csv(output / 'inventory.csv'):
        if row['kind'] == 'cells' and row['offset'] not in paired:
            unresolved.append(dict(cells=row['offset'], tiles='',
                                   issue='No standalone tile-bank association; may share banks or require live VRAM',
                                   evidence=row['keys']))
    for row in sprites:
        if not row['palette_evidence'].startswith('verified') or row['warnings']:
            unresolved.append(dict(cells=row['cells'], tiles=row['tiles'],
                                   issue='; '.join(filter(None, (row['palette_evidence'], row['warnings']))),
                                   evidence=row['evidence']))
    with (output / 'unresolved.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=('cells', 'tiles', 'issue', 'evidence'))
        writer.writeheader()
        writer.writerows(unresolved)

    labels = {'confirmed': 'Japanese', 'possible': 'Possible Japanese',
              'none': 'No Japanese seen', 'unreviewed': 'Not reviewed'}
    parts = ['''<!doctype html><meta charset="utf-8"><title>Slime graphics</title>
<style>
body{font:16px system-ui;margin:24px;background:#19252f;color:#ecf0f1}
a{color:#8bd6ff}input,select{font:inherit;padding:8px;margin:4px}header{position:sticky;top:0;background:#19252f;padding:8px;z-index:1}
main{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:20px}
article{background:#263947;padding:16px;overflow:hidden}article img{max-width:100%;max-height:420px;object-fit:contain;image-rendering:pixelated;background:#34495e}
article h2{font-size:18px;margin-top:0}small{display:block;margin:8px 0;color:#c2cbd2}.badge{color:#91e8b3}details{font-size:13px}p{max-width:1000px}article[hidden]{display:none}
</style><h1>Slime graphics</h1>
<p>Assembled backgrounds and sprite cells. World maps show base palette variant 0, without actors or event/animation substitutions. UI layers omit runtime text and counters. Sprite sheets preserve individual animation cells; they are not full scene screenshots.</p>
<p>Unresolved palette/layout associations are hidden by default. “Not reviewed” is not a finding of no Japanese. <a href="inventory.csv">Resource inventory</a> · <a href="backgrounds/backgrounds.csv">Background mappings</a> · <a href="sprites/sprites.csv">Sprite mappings</a> · <a href="text-review.csv">Text review</a> · <a href="unresolved.csv">Unresolved associations</a></p>
<header><input id="q" placeholder="Search name, offset or finding" size="30">
<select id="category"><option value="">All types</option><option>World</option><option>Background</option><option>UI</option><option>Sprites</option></select>
<select id="text"><option value="">All review statuses</option><option value="confirmed" selected>Japanese</option><option value="possible">Possible Japanese</option><option value="unreviewed">Not reviewed</option><option value="none">No Japanese seen</option></select>
<label><input type="checkbox" id="uncertain">Include unresolved palettes/layouts</label><span id="count"></span></header><main>''']
    for card in cards:
        esc = html.escape
        paths = card['paths']
        parts.append(f'<article data-group="{esc(card["group"])}" data-status="{card["status"]}" data-known="{int(card["known"])}">'
                     f'<h2>{esc(card["name"])}</h2><span class="badge">{labels[card["status"]]}</span>'
                     f'<small>{esc(card["offsets"])}</small>'
                     f'<a href="{esc(paths[0])}"><img loading="lazy" src="{esc(paths[0])}"></a>'
                     f'<p>{esc(card["note"])}</p><details><summary>Resources and other sheets</summary>'
                     f'<p>{esc(card["evidence"])}</p>')
        parts.extend(f'<a href="{esc(p)}">{esc(Path(p).name)}</a> ' for p in paths)
        parts.append('</details></article>')
    parts.append('''</main><script>
const q=document.querySelector('#q'),category=document.querySelector('#category'),text=document.querySelector('#text'),uncertain=document.querySelector('#uncertain');
function filter(){let n=0;document.querySelectorAll('article').forEach(a=>{a.hidden=(!uncertain.checked&&a.dataset.known!=='1')||(category.value&&a.dataset.group!==category.value)||(text.value&&a.dataset.status!==text.value)||!a.textContent.toLowerCase().includes(q.value.toLowerCase());if(!a.hidden)n++});document.querySelector('#count').textContent=n+' previews'}
for(const e of [q,category,text,uncertain])e.addEventListener('input',filter);filter();
</script>''')
    (output / 'index.html').write_text('\n'.join(parts))
    print(f'{len(cards)} reconstructed preview groups; gallery: {output / "index.html"}')


def extract_full(rom, output):
    from graphics_backgrounds import export_backgrounds
    from graphics_sprites import export_sprites, parse_cells
    from graphics_ui import export_ui
    from graphics_pot import extract as export_pot
    nodes = discover(rom)
    for node in nodes.values():
        if parse_cells(node['raw']):
            node['kind'] = 'cells'
    export_resources(nodes, output)
    extract(rom, output / 'known')
    export_backgrounds(rom, output / 'backgrounds')
    export_sprites(rom, output / 'sprites')
    export_ui(rom, output / 'ui')
    export_pot(rom, output / 'pot-title')
    gallery(output)


def archive_entries(rom, offset):
    count = struct.unpack_from('<I', rom, offset)[0]
    base = offset + 4 + count * 8
    if not 0 < count < 0x8000 or base > len(rom):
        raise ValueError(f'Invalid archive at {offset:06x}')
    for i in range(count):
        relative, size = struct.unpack_from('<II', rom, offset + 4 + i * 8)
        start = base + relative
        if start + size > len(rom):
            raise ValueError(f'Archive entry {offset:06x}/{i:03x} outside ROM')
        yield i, start, size


def render_bg(tiles, tilemap, palette, palette_bank=0):
    from PIL import Image
    colors = [gba_bgr555_to_rgba(c) for c, in struct.iter_unpack('<H', palette)]
    if len(tilemap) % 64:
        raise ValueError('Expected a 32-column text BG map')
    image = Image.new('RGBA', (256, len(tilemap) // 64 * 8))
    for i, (entry,) in enumerate(struct.iter_unpack('<H', tilemap)):
        tile, bank = entry & 1023, (entry >> 12) - palette_bank
        if tile * 32 + 32 > len(tiles) or bank < 0 or bank * 16 + 16 > len(colors):
            raise ValueError(f'Tile/palette outside supplied BG resources: {entry:04x}')
        for y in range(8):
            for x in range(8):
                sx, sy = (7 - x if entry & 1024 else x), (7 - y if entry & 2048 else y)
                value = tiles[tile * 32 + sy * 4 + sx // 2]
                index = (value >> (4 * (sx & 1))) & 15
                color = colors[bank * 16 + index] if index else (0, 0, 0, 0)
                image.putpixel((i % 32 * 8 + x, i // 32 * 8 + y), color)
    return image


def export_title_cells(tiles, cells, palette, output):
    """08000FFC consumes a u16 count followed by six-byte OAM records per cell."""
    from PIL import Image, ImageDraw
    colors = [gba_bgr555_to_rgba(v) for v, in struct.iter_unpack('<H', palette)]
    shapes = (((8, 8), (16, 16), (32, 32), (64, 64)),
              ((16, 8), (32, 8), (32, 16), (64, 32)),
              ((8, 16), (8, 32), (16, 32), (32, 64)))
    count = struct.unpack_from('<H', cells, 2)[0]
    sheet = Image.new('RGBA', (1280, 160 * ((count + 4) // 5)), (50, 73, 95, 255))
    labels = ImageDraw.Draw(sheet)
    for i in range(count):
        pos = 4 + struct.unpack_from('<H', cells, 4 + 2 * i)[0]
        parts = struct.unpack_from('<H', cells, pos)[0]
        image = Image.new('RGBA', (256, 256))
        for j in reversed(range(parts)):
            a0, a1, a2 = struct.unpack_from('<HHH', cells, pos + 2 + 6 * j)
            if a0 & 0x2100:  # These cells are ordinary 4bpp, non-affine OBJ parts.
                raise ValueError('Unsupported affine/8bpp cell')
            width, height = shapes[a0 >> 14][a1 >> 14]
            x0, y0 = a1 & 511, a0 & 255
            x0 -= 512 if x0 >= 256 else 0
            y0 -= 256 if y0 >= 128 else 0
            for y in range(height):
                for x in range(width):
                    sx = width - 1 - x if a1 & 0x1000 else x
                    sy = height - 1 - y if a1 & 0x2000 else y
                    tile = (a2 & 1023) + (sy // 8) * (width // 8) + sx // 8
                    value = tiles[tile * 32 + (sy % 8) * 4 + (sx % 8) // 2]
                    shade = (value >> (4 * (sx & 1))) & 15
                    if shade:
                        image.putpixel((128 + x0 + x, 128 + y0 + y), colors[(a2 >> 12) * 16 + shade])
        # Preview crops only; original coordinates/animation data remain in 2be.bin.
        box = image.getbbox()
        image = image.crop(box) if box else Image.new('RGBA', (1, 1))
        image.save(output / f'title-cell-{i:02d}.png')
        image.thumbnail((256, 140))
        x, y = i % 5 * 256, i // 5 * 160
        labels.text((x, y), str(i), fill='white')
        sheet.alpha_composite(image, (x, y + 16))
    sheet.save(output / 'title-sprites.png')


def extract(rom, output):
    output.mkdir(parents=True, exist_ok=True)
    rows, entries, decoded = [], {}, {}
    for archive in ARCHIVES:
        directory = output / f'{archive:06x}'
        directory.mkdir(exist_ok=True)
        for index, start, size in archive_entries(rom, archive):
            data = rom[start:start + size]
            key = (archive, index)
            entries[key] = data
            stem = directory / f'{index:03x}'
            stem.with_suffix('.bin').write_bytes(data)
            expanded = None
            if data[:1] == b'\x70':
                try:
                    expanded, mode, end = Decompressor(data, 0, max_output=0x40000).decompress()
                except (DecodeError, ValueError, IndexError):
                    pass
            if expanded is not None:
                decoded[key] = expanded
                stem.with_suffix('.decoded').write_bytes(expanded)
            rows.append((f'{archive:06x}', f'{index:03x}', f'{start:06x}', size,
                         len(expanded) if expanded is not None else '',
                         'indexed directory; content type requires consumer analysis'))
    with (output / 'inventory.csv').open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(('archive', 'id', 'rom_offset', 'stored_bytes', 'decoded_bytes', 'evidence'))
        writer.writerows(rows)

    def get(index):
        key = (0x765FA8, index)
        return decoded.get(key, entries[key])

    # 080D4426..080D445A: logo/press-start tiles, palette, BG map.
    # 080D3C74..080D3C90: scenery tiles/map (palette supplied separately).
    for name, tiles, tilemap, palette in (
            ('title-layer', get(0x19F), get(0x1A1), get(0x1A0)),
            ('title-background', get(0x1A2), get(0x1A3), get(0x1A4))):
        render_bg(tiles, tilemap, palette).save(output / f'{name}.png')
        for suffix, data in (('4bpp', tiles), ('map', tilemap), ('pal', palette)):
            (output / f'{name}.{suffix}').write_bytes(data)

    # 080D4484..080D44BA loads these tiles, OBJ palettes and cell/animation data.
    export_title_cells(get(0x2BD), get(0x2BE), get(0x2A7), output)

    # 08081160 and 080C5EC8: literal base plus hardcoded map/palette offsets.
    # This bundle has no entry in the five archives above.
    tiles, _, end_tiles = Decompressor(rom, 0x558BCC).decompress()
    tilemap, _, end_map = Decompressor(rom, 0x5597D4).decompress()
    assert end_tiles == 0x5597D4 and end_map == 0x559A34
    palette = rom[0x559A34:0x559A54]
    render_bg(tiles, tilemap, palette, palette_bank=2).crop((0, 0, 240, 160)).save(output / 'tutorial.png')
    for suffix, data in (('4bpp', tiles), ('map', tilemap), ('pal', palette)):
        (output / f'tutorial.{suffix}').write_bytes(data)
    # Preserve the original packed bundle too; both callers derive the interior addresses.
    (output / 'tutorial.bundle').write_bytes(rom[0x558BCC:0x559A54])
    print(f'{len(rows)} indexed entries; {len(decoded)} decodable streams; previews in {output}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rom', type=Path)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--full', action='store_true', help='Include scene tables, nested archives and sprite reconstruction')
    parser.add_argument('--gallery-only', action='store_true', help='Rebuild gallery from existing local exports and review CSV')
    args = parser.parse_args()
    output = args.output or Path('dist/graphics-full' if args.full or args.gallery_only else 'dist/graphics')
    if args.gallery_only:
        gallery(output)
    else:
        (extract_full if args.full else extract)(args.rom.read_bytes(), output)
