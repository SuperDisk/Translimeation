"""Reconstruct static UI layers whose resource loads have been traced in code."""
import csv
from pathlib import Path

from audit_graphics import archive_entries
from graphics_backgrounds import render_map
from slime_gfx import Decompressor


def export_ui(rom, output):
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    entries = {i: (p, n) for i, p, n in archive_entries(rom, 0x765FA8)}

    def get(index):
        p, n = entries[index]
        data = rom[p:p + n]
        return Decompressor(data, 0).decompress()[0] if data[:1] == b'\x70' else data

    # IDs are hexadecimal. These are static BG layers; menu choices, counters,
    # room markers and text rendered into RAM are separate runtime overlays.
    configurations = [
        ('main-menu-early', 0xDF, 0xE0, [0xD4], '080c70e0..080c716a'),
        ('main-menu-later', 0xDF, 0xE0, [0xD3], '080c70e0..080c716a'),
        ('town-menu', 0x1C6, 0x1C7, [0x1E4], '080c72c0..080c72f2'),
        ('rescue-menu', 0x169, 0x16B, [0x16E, 0x16D, 0x170, 0x171], '080bb95c..080bb9d0'),
        ('ranking-a', 0x132, 0x133, [0x134, 0x136], '080c2392..080c23f0'),
        ('ranking-b', 0x132, 0x133, [0x139, 0x13A], '080c2392..080c23be; 080c256a..080c2584'),
    ]
    rows = []
    for name, tiles_id, palette_id, map_ids, evidence in configurations:
        tiles = bytearray(get(tiles_id).ljust(0x8000, b'\0'))
        if tiles_id == 0xDF:
            # 080c7d80..080c7db4 initializes the runtime number slots from tile 63.
            for start in (0x3C0, 0x3E0):
                for i in range(24):
                    tiles[(start + i) * 32:(start + i + 1) * 32] = tiles[0x7E0:0x800]
        palette = b'\0\0' + get(palette_id)  # decoded to 05000002
        for map_id in map_ids:
            tilemap = get(map_id)
            image = render_map(tiles, tilemap, palette, 32)
            path = f'{name}-{map_id:03x}.png'
            image.save(output / path)
            rows.append(dict(name=f'{name}/{map_id:03x}', preview=path,
                             tiles=f'{entries[tiles_id][0]:06x}', tilemap=f'{entries[map_id][0]:06x}',
                             palette=f'{entries[palette_id][0]:06x}', evidence=evidence,
                             note='Static BG layer only; runtime text, counters and sprite overlays omitted'))
    # Save/link/help menu: 080D5EBC loads palette into the shadow palette at
    # 0200F3F0 (= bank 4 of 0200F370), then tiles from base+E0 to 06000000.
    # The screen handlers below load these maps into the same BG character bank.
    tiles_offset, palette_offset = 0x751B90, 0x751AB0
    tiles = Decompressor(rom, tiles_offset).decompress()[0]
    palette = bytes(128) + Decompressor(rom, palette_offset).decompress()[0]
    for offset, evidence in (
            (0x756464, '080d6652'), (0x75651C, '080d66b4'),
            (0x7565BC, '080d733c literal'), (0x756770, '080d6a00 literal'),
            (0x756890, '080d75ac literal'), (0x756A58, '080d6d1c literal'),
            (0x756C58, 'map table 087d1c5c[0]'), (0x756E70, 'map table 087d1c5c[1]'),
            (0x757030, 'map table 087d1c5c[2]'), (0x757788, '080d7558 literal')):
        image = render_map(tiles, Decompressor(rom, offset).decompress()[0], palette, 32)
        path = f'file-menu-{offset:06x}.png'
        image.save(output / path)
        rows.append(dict(name=f'file-menu/{offset:06x}', preview=path,
                         tiles=f'{tiles_offset:06x}', tilemap=f'{offset:06x}',
                         palette=f'{palette_offset:06x}', evidence='080d5ebc..080d5ece; ' + evidence,
                         note='Static menu message layer; selection sprites and runtime text omitted'))
    with (output / 'ui.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    return rows
