#!/usr/bin/env python3
"""Reconstruct archive sprite cells; keep proven and inferred associations separate."""
import argparse
import csv
import hashlib
import struct
from pathlib import Path

import _paths
from slime_gfx import Decompressor, DecodeError, gba_bgr555_to_rgba
from PIL import Image, ImageDraw

ROOTS = (0x1D9FEC, 0x44D550, 0x484474, 0x54B4FC, 0x765FA8)
SHAPES = (((8, 8), (16, 16), (32, 32), (64, 64)),
          ((16, 8), (32, 8), (32, 16), (64, 32)),
          ((8, 16), (8, 32), (16, 32), (32, 64)))


def parse_cells(data):
    """Return original OAM triples per cell, or None for an invalid directory."""
    if len(data) < 8:
        return None
    end, count = struct.unpack_from('<HH', data)
    end &= ~1
    if not 0 < count <= 512 or not 4 + count * 2 <= end < len(data):
        return None
    cells, last = [], 0
    for i in range(count):
        pos = 4 + (struct.unpack_from('<H', data, 4 + i * 2)[0] & ~1)
        if not 4 + count * 2 <= pos <= end - 2:
            return None
        n = struct.unpack_from('<H', data, pos)[0]
        if n > 128 or pos + 2 + n * 6 > end:
            return None
        parts = list(struct.iter_unpack('<HHH', data[pos + 2:pos + 2 + n * 6]))
        if any(a0 >> 14 == 3 for a0, _, _ in parts):
            return None
        cells.append(parts)
        last = max(last, pos + 2 + n * 6)
    return cells if last == end else None


def render_cell(tiles, parts, palette=None, force_palette=None):
    """Return (RGBA image, origin x/y, warnings); never clip negative coordinates.

    Coordinates and source OAM remain separately exported. Affine transformations
    need runtime matrices, so these parts are shown at their untransformed size.
    Missing palette banks use labeled grayscale, never a repeated guessed bank.
    """
    warnings, boxes = set(), []
    for a0, a1, a2 in parts:
        if a0 & 0x200 and not a0 & 0x100:  # disabled non-affine OBJ
            continue
        width, height = SHAPES[a0 >> 14][a1 >> 14]
        x, y = a1 & 255, a0 & 255
        x -= 256 if x >= 128 else 0
        y -= 256 if y >= 128 else 0
        boxes.append((x, y, width, height, a0, a1, a2))
    if not boxes:
        return None, (0, 0), []
    x0, y0 = min(b[0] for b in boxes), min(b[1] for b in boxes)
    x1, y1 = max(b[0] + b[2] for b in boxes), max(b[1] + b[3] for b in boxes)
    image = Image.new('RGBA', (x1 - x0, y1 - y0))
    colors = [gba_bgr555_to_rgba(c) for c, in struct.iter_unpack('<H', palette or b'')]
    for x, y, width, height, a0, a1, a2 in reversed(boxes):
        if a0 & 0x100:
            warnings.add('affine matrix not available; untransformed preview')
        bpp = 8 if a0 & 0x2000 else 4
        tile_bytes = 64 if bpp == 8 else 32
        for dy in range(height):
            for dx in range(width):
                sx = width - 1 - dx if not a0 & 0x100 and a1 & 0x1000 else dx
                sy = height - 1 - dy if not a0 & 0x100 and a1 & 0x2000 else dy
                offset = (a2 & 1023) * 32 + ((sy // 8) * (width // 8) + sx // 8) * tile_bytes
                offset += (sy % 8) * bpp + (sx % 8) * bpp // 8
                if offset >= len(tiles):
                    warnings.add('tile reference outside supplied bank')
                    continue
                shade = tiles[offset] if bpp == 8 else (tiles[offset] >> ((sx & 1) * 4)) & 15
                if not shade:
                    continue
                bank = a2 >> 12 if force_palette is None else force_palette
                index = shade + (0 if bpp == 8 else bank * 16)
                if index < len(colors):
                    color = colors[index]
                else:
                    warnings.add('grayscale fallback: palette bank unknown')
                    level = 32 + shade * 223 // (255 if bpp == 8 else 15)
                    color = (level, level, level, 255)
                image.putpixel((x + dx - x0, y + dy - y0), color)
    box = image.getbbox()
    if not box:
        return None, (x0, y0), sorted(warnings)
    return image.crop(box), (x0 + box[0], y0 + box[1]), sorted(warnings)


def _entries(rom, root):
    count = struct.unpack_from('<I', rom, root)[0]
    if not 0 < count < 1000:
        raise ValueError('not an archive')
    base = root + 4 + count * 8
    result = []
    for i in range(count):
        offset, size = struct.unpack_from('<II', rom, root + 4 + i * 8)
        start = base + offset
        if start + size > len(rom):
            raise ValueError('archive entry outside ROM')
        result.append((i, start, size))
    return result


def _archives(rom, roots):
    groups = {}
    def visit(root, key, length=None):
        try:
            entries = _entries(rom, root)
        except (ValueError, struct.error):
            return
        if length is not None:
            base = root + 4 + len(entries) * 8
            if entries[0][1] != base or not length - 3 <= max(p + n for _, p, n in entries) - root <= length:
                return
        groups[key] = entries
        for index, pos, size in entries:
            if size >= 12:
                visit(pos, f'{key}/{index:03x}', size)
    for root in roots:
        visit(root, f'{root:06x}')
    return groups


def _expand(data):
    if data[:1] == b'\x70':
        try:
            return Decompressor(data, 0, max_output=0x40000).decompress()[0]
        except (ValueError, IndexError, DecodeError):
            pass
    return data


def sprite_associations(rom, roots=ROOTS):
    """Return association dicts including byte offsets and explicit evidence."""
    groups = _archives(rom, roots)
    result, covered = [], set()
    def emit(key, tile_id, cell_id, palette_id, evidence, palette_evidence, pal_skip=0, pal_size=None, force_palette=None):
        entries = groups.get(key)
        if entries is None or not 0 <= tile_id < len(entries) or not 0 <= cell_id < len(entries):
            return
        _, tp, tn = entries[tile_id]
        _, cp, cn = entries[cell_id]
        if parse_cells(rom[cp:cp + cn]) is None:
            return
        pp, pn = 0, 0
        if palette_id is not None and 0 <= palette_id < len(entries):
            _, pp, pn = entries[palette_id]
            pp += pal_skip
            pn -= pal_skip
            if pal_size is not None:
                pn = min(pn, pal_size)
        if pn < 0:
            return
        if palette_id is not None and not pp:
            palette_evidence = 'descriptor palette ID outside nested archive; special loader unresolved; grayscale fallback'
        association = dict(archive=key, tiles=f'{tp:06x}', cells=f'{cp:06x}',
                           palette=f'{pp:06x}' if pp else '', tiles_id=f'{tile_id:03x}',
                           cells_id=f'{cell_id:03x}', palette_id=f'{palette_id:03x}' if palette_id is not None else '',
                           tiles_size=tn, cells_size=cn, palette_size=pn,
                           evidence=evidence, palette_evidence=palette_evidence,
                           palette_mode='actor override' if evidence.startswith('actor ') else 'cell banks',
                           force_palette=0 if evidence.startswith('actor ') else force_palette)
        identity = (tp, cp, pp, pn)
        if identity not in covered:
            covered.add(identity)
            result.append(association)

    # Actor IDs 9..217 dispatch through 0802B580; earlier IDs use 484474.
    # 0802B994: actor descriptor = 081CB604 + ID*48.
    # 0802BB44..8E: nested tiles=descriptor+6, cells=tiles+1.
    # 0802C1B4..D0: palette=descriptor+4, copies one 32-byte bank.
    # 0802BB64..6E sets context palette; 08001164..78 overrides OAM bank.
    # 080010A6..AA also confirms source X is signed *8*-bit, not hardware 9-bit.
    targets = {0x0802B8C4: 0x1D9FEC, 0x0802B8CC: 0x54B4FC, 0x0802B8D4: 0x484474}
    for actor in range(218):
        target = struct.unpack_from('<I', rom, 0x2B580 + (actor - 9) * 4)[0] if actor >= 9 else 0x0802B8D4
        root = targets.get(target)
        if root not in roots:
            continue
        nested, _, palette_id, tile_id = struct.unpack_from('<4H', rom, 0x1CB604 + actor * 48)
        key = f'{root:06x}/{nested:03x}'
        emit(key, tile_id, tile_id + 1, palette_id,
             f'actor {actor:02x}: descriptor {0x1CB604 + actor * 48:06x}; loaders 0802B564/0802BB44',
             'verified descriptor +4; palette loader 0802C1B4 copies first bank', pal_size=32)

    # Title screen resources traced at 080D4484..080D44BA.
    emit('765fa8', 0x2BD, 0x2BE, 0x2A7, 'title loader 080D4484..080D44BA',
         'verified title OBJ palette load', pal_size=160)
    # The file-menu palette is itself compressed. Adjacency alone otherwise
    # mistakes that palette for the tiles immediately preceding the cell data.
    emit('765fa8', 0x219, 0x21B, 0x21A, 'file-menu loader 080D5F3C..080D5F76',
         'verified OBJ tiles, compressed palette and cells', pal_size=512)
    # Object 93 loads bank 15 explicitly; slot 57 binds tiles/cells at 087D1A2C.
    emit('1d9fec', 0x0BC, 0x0BE, 0x0BD,
         'pot logo object 93; loader 080D1BC4; slot 57 descriptor 087D1A2C',
         'verified palette copied to bank 15 at 080D1BC4..080D1BD0',
         pal_size=32, force_palette=0)
    # Duplicate uses palette bank 4 except cell 10 part 14, which specifies bank 1.
    # Preserve this source difference instead of forcing it to match the original.
    emit('765fa8', 0x2C2, 0x2C3, 0x2C1,
         'pot logo duplicate: exact tiles/cell geometry and timing match 1d9fec/0bc,0be',
         'bank 4 of palette 2c1 equals verified pot bank 15; duplicate loader untraced',
         pal_size=512)
    # Resident common OBJ tiles occupy 06010000..06011FFF. Scene startup
    # 0800F1A8..0800F1CE loads 136 with palette 137 or 138 depending on state.
    # These nonadjacent cell sets use that bank; some individual consumers are
    # traced (111DE, 18432, 2F5D2, 228F6), others retain inferred binding status.
    for cell_id in (0x129, 0x12A, *range(0x12C, 0x136)):
        emit('1d9fec', 0x136, cell_id, 0x137,
             'shared resident OBJ bank loaded at 0800F1C8; cell binding inferred from bank range/layout',
             'verified common palette load 0800F1A8..0800F1BE; default 137 (alternate state uses 138)')
    emit('765fa8', 0x27F, 0x277, None,
         'verified UI loader 080C824E..080C827E: tiles 27F, cells 277, VRAM tile base 280',
         'scene palette unresolved; grayscale fallback')
    emit('765fa8', 0x27F, 0x278, None,
         'inferred shared UI tiles 27F from adjacent cell 277 and matching tile references',
         'scene palette unresolved; grayscale fallback')
    assigned_cells = {a['cells'] for a in result}
    # Text-window cells 139 (0809B93E) reference OBJ tiles 118 and above,
    # beyond the shared 136 tile bank. They require live VRAM context; their
    # preceding entries 137/138 are compressed palettes, not their graphics.
    # The cell bytes remain in the full raw archive export.
    assigned_cells.add('40a1ec')
    for key, entries in groups.items():
        for i, cp, cn in entries:
            if f'{cp:06x}' in assigned_cells or parse_cells(rom[cp:cp + cn]) is None:
                continue
            if i == 0:
                continue
            # Common remaining pattern: tiles, optional palette, cells.
            tile_id, palette_id = i - 1, None
            _, pp, pn = entries[i - 1]
            pdata = rom[pp:pp + pn]
            if i >= 2 and 32 <= pn <= 512 and pn % 32 == 0 and pdata[:1] != b'\x70' and parse_cells(pdata) is None:
                _, tp, tn = entries[i - 2]
                if tn > pn and parse_cells(rom[tp:tp + tn]) is None:
                    tile_id, palette_id = i - 2, i - 1
            _, tp, tn = entries[tile_id]
            data = _expand(rom[tp:tp + tn])
            if len(data) % 32 or len(data) < 32 or parse_cells(data) is not None:
                continue
            emit(key, tile_id, i, palette_id,
                 'inferred adjacent tile/cell entries; OAM tile bounds checked during rendering',
                 'inferred adjacent palette; not verified by loader' if palette_id is not None else 'unknown; grayscale fallback')
    return result


def export_sprites(rom, output, roots=ROOTS):
    """Write assembled PNGs, per-cell coordinates/OAM CSV, sheets and mapping CSV.

    Returns mapping rows. Sheet paths are relative to output. Identical nonempty
    cells share an image; blank cells remain in cell CSV without empty PNGs.
    """
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    mappings = sprite_associations(rom, roots)
    for bank_number, row in enumerate(mappings):
        name = row['cells'] + ('-' + row['palette'] if row['palette'] else '-gray')
        directory = output / name
        directory.mkdir(exist_ok=True)
        tp, cp = int(row['tiles'], 16), int(row['cells'], 16)
        tiles = _expand(rom[tp:tp + row['tiles_size']])
        cell_data = rom[cp:cp + row['cells_size']]
        palette = None
        if row['palette']:
            pp = int(row['palette'], 16)
            palette = _expand(rom[pp:pp + row['palette_size']])
        metadata, images, hashes, warnings = [], [], {}, set()
        for index, parts in enumerate(parse_cells(cell_data)):
            im, (x, y), issues = render_cell(tiles, parts, palette,
                force_palette=row['force_palette'])
            warnings.update(issues)
            path = ''
            if im is not None:
                digest = hashlib.sha256(struct.pack('<II', *im.size) + im.tobytes()).digest()
                path = hashes.get(digest)
                if path is None:
                    path = f'cell-{index:03d}.png'
                    hashes[digest] = path
                    im.save(directory / path)
                    images.append((index, im))
            metadata.append(dict(cell=index, x=x, y=y, width=im.width if im else 0,
                                 height=im.height if im else 0, image=path or '',
                                 warnings='; '.join(issues),
                                 oam=';'.join(f'{a:04x},{b:04x},{c:04x}' for a,b,c in parts)))
        with (directory / 'cells.csv').open('w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=list(metadata[0]))
            writer.writeheader(); writer.writerows(metadata)
        sheets = []
        for start in range(0, len(images), 36):
            page = images[start:start + 36]
            sheet = Image.new('RGB', (1200, 30 + 150 * ((len(page) + 5) // 6)), '#34495e')
            draw = ImageDraw.Draw(sheet)
            label = row['tiles'] + ' / ' + row['cells'] + ' | ' + row['palette_evidence']
            draw.text((8, 8), label, fill='white')
            for j, (index, im) in enumerate(page):
                x, y = j % 6 * 200, 30 + j // 6 * 150
                draw.text((x + 5, y + 3), str(index), fill='white')
                scale = min(3, 190 / im.width, 125 / im.height)
                preview = im.resize((max(1, int(im.width * scale)), max(1, int(im.height * scale))), Image.Resampling.NEAREST)
                sheet.paste(preview, (x + 5, y + 20), preview)
            path = f'{name}/sheet-{start // 36:02d}.png'
            sheet.save(output / path)
            sheets.append(path)
        row.update(cell_count=len(metadata), unique_images=len(images), sheets=';'.join(sheets),
                   warnings='; '.join(sorted(warnings)), coordinates=f'{name}/cells.csv')
    with (output / 'sprites.csv').open('w', newline='') as f:
        if mappings:
            writer = csv.DictWriter(f, fieldnames=list(mappings[0]))
            writer.writeheader(); writer.writerows(mappings)
    return mappings


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rom', type=Path)
    parser.add_argument('--output', type=Path, default=Path('dist/graphics-full/sprites'))
    args = parser.parse_args()
    rows = export_sprites(args.rom.read_bytes(), args.output)
    print(f'{len(rows)} sprite associations; {sum(r["unique_images"] for r in rows)} unique cell previews')
