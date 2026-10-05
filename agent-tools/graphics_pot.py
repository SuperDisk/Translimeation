#!/usr/bin/env python3
"""Reconstruct the pot-minigame title from its BG and sprite resources."""
import _paths

import argparse
import csv
import struct
from pathlib import Path

from PIL import Image
from audit_graphics import archive_entries, render_bg
from slime_gfx import Decompressor, gba_bgr555_to_rgba

SHAPES = (((8, 8), (16, 16), (32, 32), (64, 64)),
          ((16, 8), (32, 8), (32, 16), (64, 32)),
          ((8, 16), (8, 32), (16, 32), (32, 64)))


def resource(rom, archive, index):
    _, start, size = next(e for e in archive_entries(rom, archive) if e[0] == index)
    raw = rom[start:start + size]
    decoded = Decompressor(raw, 0).decompress()[0] if raw[0] == 0x70 else raw
    return start, raw, decoded


def cell_image(tiles, cells, palette, index, anchor=(120, 80)):
    """Ordinary 4bpp OBJ cells, on the original 240x160 scene coordinate grid."""
    image = Image.new('RGBA', (240, 160))
    colors = [gba_bgr555_to_rgba(v) for v, in struct.iter_unpack('<H', palette)]
    count = struct.unpack_from('<H', cells, 2)[0]
    assert index < count
    pos = 4 + (struct.unpack_from('<H', cells, 4 + 2 * index)[0] & ~1)
    parts = struct.unpack_from('<H', cells, pos)[0]
    for j in reversed(range(parts)):
        a0, a1, a2 = struct.unpack_from('<HHH', cells, pos + 2 + 6 * j)
        assert not a0 & 0x2100
        width, height = SHAPES[a0 >> 14][a1 >> 14]
        x0, y0 = a1 & 255, a0 & 255  # 080016BE and 0800173C sign-extend bytes.
        x0 -= 256 if x0 >= 128 else 0
        y0 -= 256 if y0 >= 128 else 0
        for y in range(height):
            for x in range(width):
                sx = width - 1 - x if a1 & 0x1000 else x
                sy = height - 1 - y if a1 & 0x2000 else y
                tile = (a2 & 1023) + (sy // 8) * (width // 8) + sx // 8
                value = tiles[tile * 32 + (sy % 8) * 4 + (sx % 8) // 2]
                shade = value >> (4 * (sx & 1)) & 15
                dx, dy = anchor[0] + x0 + x, anchor[1] + y0 + y
                if shade and 0 <= dx < 240 and 0 <= dy < 160:
                    image.putpixel((dx, dy), colors[(a2 >> 12) * 16 + shade])
    return image


def animation(cells, index=0):
    # 08002140 resolves offsets relative to the animation offset table; the
    # two preceding words are the nominal anchor, followed by command count.
    base = (struct.unpack_from('<H', cells)[0] & ~1) + 2
    offset = struct.unpack_from('<H', cells, base + index * 2)[0] & ~1
    pos = base + offset
    x, y, count = struct.unpack_from('<HHH', cells, pos)
    commands = [struct.unpack_from('<HH', cells, pos + 6 + i * 4)
                for i in range(count)]
    return (x, y), commands


def timeline(commands, ticks):
    # Literal behavior of 0800188C, called before drawing by 08002260:
    # decrement the byte timer, advance on zero, stop on a zero-duration row.
    index, remaining, running = 0, commands[0][1] & 255, bool(commands)
    for _ in range(ticks):
        if running:
            remaining = (remaining - 1) & 255
            if not remaining:
                index = (index + 1) % len(commands)
                remaining = commands[index][1] & 255
                running = bool(remaining)
        yield commands[index][0]


def extract(rom, output):
    output.mkdir(parents=True, exist_ok=True)
    records = []
    def get(name, archive, index):
        start, raw, decoded = resource(rom, archive, index)
        (output / f'{name}.bin').write_bytes(raw)
        (output / f'{name}.decoded').write_bytes(decoded)
        records.append((name, f'{archive:06x}/{index:03x}', f'{start:06x}', len(raw), len(decoded)))
        return decoded

    # 080C5980..080C599C: BG tiles ->06008000, initial map ->0600F800.
    bg_tiles = get('pot-tiles', 0x765FA8, 0x1BA)
    bg_palette = get('pot-palette', 0x765FA8, 0x1BB)
    initial_map = get('pot-map', 0x765FA8, 0x1BC)
    replacement = get('pot-crack-map', 0x765FA8, 0x1BD)
    cracked_map = bytearray(initial_map)
    # 080C5BA6..080C5BC2: timer 129 writes a 16x16 map at0600F8CE (x7,y3).
    for y in range(16):
        at = ((y + 3) * 32 + 7) * 2
        cracked_map[at:at + 32] = replacement[y * 32:y * 32 + 32]
    backgrounds = [render_bg(bg_tiles, m, bg_palette).crop((0, 0, 240, 160))
                   for m in (initial_map, cracked_map)]

    # Object 93 (init080D1B8C) selects slot 57/animation 0 at080D1BD4.
    # Startup copy 087D17C0->03001F30 puts the descriptor 087D1A2C at 0300219C.
    assert struct.unpack_from('<HH', rom, 0x7D1A2C) == (0xBC, 0xBE)
    tiles = get('lettering-tiles', 0x1D9FEC, 0xBC)
    palette = get('lettering-palette', 0x1D9FEC, 0xBD)
    cells = get('lettering-cells', 0x1D9FEC, 0xBE)
    # 080D1BC4..080D1BD0 copies this 32-byte palette to OBJ bank 15.
    palette = bytes(15 * 32) + palette
    # The separate 765FA8 tiles are identical. Its cell bank differs only in
    # palette selection (bank 4 instead of 15); geometry and timing match.
    assert resource(rom, 0x765FA8, 0x2C2)[2] == tiles
    duplicate = bytearray(resource(rom, 0x765FA8, 0x2C3)[2])
    for i in range(struct.unpack_from('<H', duplicate, 2)[0]):
        pos = 4 + (struct.unpack_from('<H', duplicate, 4 + 2 * i)[0] & ~1)
        for j in range(struct.unpack_from('<H', duplicate, pos)[0]):
            duplicate[pos + 7 + 6 * j] |= 0xF0
    assert bytes(duplicate) == cells
    anchor, commands = animation(cells)
    assert anchor == (120, 80)  # Also fixed explicitly by 080D1C2C..080D1C30.
    images = [cell_image(tiles, cells, palette, i, anchor)
              for i in range(struct.unpack_from('<H', cells, 2)[0])]
    for i, image in enumerate(images):
        image.save(output / f'lettering-cell-{i:02d}.png')
    backgrounds[0].save(output / 'pot-intact.png')
    backgrounds[1].save(output / 'pot-cracked.png')
    assembled = backgrounds[1].copy()
    assembled.alpha_composite(images[13])
    assembled.save(output / 'pot-title.png')

    # Scene timer reaches 190 then clears the title BG/object. Show precisely
    # that interval. Actor/world scenery is intentionally absent from layers.
    frames = []
    rows = []
    for tick, cell in enumerate(timeline(commands, 190), 1):
        frame = backgrounds[int(tick >= 129)].copy()
        frame.alpha_composite(images[cell])
        if tick == 190:
            frame = Image.new('RGBA', (240, 160))
        frames.append(frame)
        rows.append((tick, cell, int(tick >= 129), int(tick < 190)))
    # GIF centiseconds cannot represent every GBA frame; alternate 10/20 ms.
    durations = [round((i + 1) * 100 / 60) * 10 - round(i * 100 / 60) * 10
                 for i in range(len(frames))]
    frames[0].save(output / 'pot-title.gif', save_all=True, append_images=frames[1:],
                   duration=durations, loop=0, disposal=2)
    with (output / 'resources.csv').open('w', newline='') as f:
        w = csv.writer(f)
        w.writerow(('name', 'archive_entry', 'rom_offset', 'stored_size', 'decoded_size'))
        w.writerows(records)
    with (output / 'frames.csv').open('w', newline='') as f:
        w = csv.writer(f)
        w.writerow(('tick', 'cell', 'cracked_bg', 'visible'))
        w.writerows(rows)
    with (output / 'animation.csv').open('w', newline='') as f:
        w = csv.writer(f)
        w.writerow(('cell', 'ticks'))
        w.writerows(commands)
    return output / 'pot-title.png'


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rom', type=Path)
    parser.add_argument('--output', type=Path, default=Path('dist/graphics-full/pot-title'))
    args = parser.parse_args()
    print(extract(args.rom.read_bytes(), args.output))
