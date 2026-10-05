#!/usr/bin/env python3
"""Reconstruct backgrounds from ROM loader tables, including 240 world scenes."""
import _paths

import argparse
import csv
import struct
from functools import lru_cache
from pathlib import Path

from PIL import Image
from slime_gfx import Decompressor, gba_bgr555_to_rgba

ROM_BASE = 0x08000000
SCENES = 0x54F198
SCENE_COUNT = 240  # Next table is 550bd8; getter 0807eb64 uses 28-byte records.


def u32(rom, offset):
    return struct.unpack_from('<I', rom, offset)[0]


def render_map(tiles, tilemap, palette, width):
    """Linear hardware halfword map, with its real palette bank and tile flips."""
    colors = [gba_bgr555_to_rgba(c) for c, in struct.iter_unpack('<H', palette)]
    if len(tilemap) % (width * 2):
        raise ValueError('Map has incomplete rows')
    output = Image.new('RGBA', (width * 8, len(tilemap) // (width * 2) * 8))

    @lru_cache(None)
    def tile_image(entry):
        tile, bank = entry & 1023, entry >> 12
        data = tiles[tile * 32:(tile + 1) * 32]
        if len(data) != 32:
            raise ValueError(f'Tile {tile} outside supplied VRAM image')
        image = Image.new('RGBA', (8, 8))
        pixels = []
        for byte in data:
            for shade in (byte & 15, byte >> 4):
                pixels.append(colors[bank * 16 + shade] if shade else (0, 0, 0, 0))
        image.putdata(pixels)
        if entry & 1024:
            image = image.transpose(Image.Transpose.FLIP_LEFT_RIGHT)
        if entry & 2048:
            image = image.transpose(Image.Transpose.FLIP_TOP_BOTTOM)
        return image

    for i, (entry,) in enumerate(struct.iter_unpack('<H', tilemap)):
        output.paste(tile_image(entry), (i % width * 8, i // width * 8))
    return output


def expand_metatiles(rom, base, map_data, width, height):
    """8098552 adds each u16 byte offset to the raw 2x2 metatile table."""
    count = width * height // 4
    if len(map_data) < count * 2:
        map_data = map_data.ljust(count * 2, b'\0')
    result = bytearray(width * height * 2)
    for i, (relative,) in enumerate(struct.iter_unpack('<H', map_data[:count * 2])):
        x, y = i % (width // 2) * 2, i // (width // 2) * 2
        entry = rom[base + relative:base + relative + 8]
        if relative & 3 or len(entry) != 8:
            raise ValueError(f'Invalid metatile offset {relative:04x}')
        top = (y * width + x) * 2
        result[top:top + 4] = entry[:4]
        result[top + width * 2:top + width * 2 + 4] = entry[4:]
    return bytes(result)


def export_backgrounds(rom, output, scene_ids=None):
    """Write PNGs and backgrounds.csv; return its row dictionaries.

    World previews use palette variant zero and unmodified scene data. Animated
    tile replacements and event-dependent room edits are not simulated.
    """
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    resources = output / 'resources'
    resources.mkdir(exist_ok=True)
    sources = {}

    def raw_resource(offset, size, kind, evidence):
        filename = f'rom-{offset:06x}-{size:x}.bin'
        (resources / filename).write_bytes(rom[offset:offset + size])
        sources[filename] = dict(file=filename, offset=f'{offset:06x}', size=size,
                                decoded_size='', kind=kind, evidence=evidence)
        return filename

    for start, end, kind in ((SCENES, SCENES + 28 * SCENE_COUNT, 'scene descriptors'),
                             (0x54DBB8, 0x54DDAC, 'palette pointers'),
                             (0x54DDAC, 0x54DE90, 'palette selectors'),
                             (0x54DE90, 0x54E008, 'tileset pointers'),
                             (0x54E008, 0x54E0C4, 'tileset selectors'),
                             (0x71174C, 0x7117B8, 'area banner descriptors'),
                             (0x558B50, 0x558B8C, 'versus banner descriptors')):
        raw_resource(start, end - start, kind, 'loader tables used in reconstruction')

    @lru_cache(None)
    def decoded(offset):
        data, _, end = Decompressor(rom, offset).decompress()
        packed = raw_resource(offset, end - offset, 'packed stream', 'exact decoder consumption')
        filename = f'rom-{offset:06x}.decoded'
        (resources / filename).write_bytes(data)
        sources[filename] = dict(file=filename, offset=f'{offset:06x}', size=end - offset,
                                decoded_size=len(data), kind='decoded stream', evidence=packed)
        return data

    def save(name, image, tiles, tilemap, palette, evidence, **extra):
        image.save(output / (name + '.png'))
        rows.append(dict(name=name, preview=name + '.png', tiles=tiles,
                         tilemap=tilemap, palette=palette, evidence=evidence, **extra))

    def bg(name, tiles_offset, map_offset, palette_offset, bank, tile_base, evidence, crop_width=240):
        tiles = bytearray(32768)
        packed = decoded(tiles_offset)
        tiles[tile_base:tile_base + len(packed)] = packed
        palette = bytearray(512)
        palette[bank * 32:(bank + 1) * 32] = rom[palette_offset:palette_offset + 32]
        tilemap = decoded(map_offset)
        raw_resource(palette_offset, 32, 'palette variant 0', 'selected BG palette bank; additional variant extents unknown')
        directory = resources / name
        directory.mkdir(exist_ok=True)
        (directory / 'tiles.4bpp').write_bytes(tiles)
        (directory / 'palette.pal').write_bytes(palette)
        (directory / 'layer-0.map').write_bytes(tilemap)
        result = render_map(tiles, tilemap, palette, 32).crop((0, 0, crop_width, len(tilemap) // 8))
        save(name, result, f'{tiles_offset:06x}', f'{map_offset:06x}',
             f'{palette_offset:06x}', evidence, resources=f'resources/{name}')

    bg('tutorial', 0x558BCC, 0x5597D4, 0x559A34, 2, 0,
       '08081160 and 080c5ec8: direct bundle; map +c08; palette +e68 at BG bank 2')
    for i in range(9):
        tiles, palette, tilemap = (v - ROM_BASE for v in struct.unpack_from('<III', rom, 0x71174C + i * 12))
        bg(f'area-{i:02d}', tiles, tilemap, palette, 3, 32,
           f'071174c[{i}]; 0809569e loads tiles at tile 1 and palette bank 3; 08095770 loads map')
    for i in range(5):
        palette, tiles, tilemap = (v - ROM_BASE for v in struct.unpack_from('<III', rom, 0x558B50 + i * 12))
        bg(f'versus-{i:02d}', tiles, tilemap, palette, 3, 0,
           f'0558b50[{i}]; 08093ef8 loads bank 3 and 32-column map strip into scrolling BGs', crop_width=256)

    for scene in (range(SCENE_COUNT) if scene_ids is None else scene_ids):
        record = SCENES + scene * 28
        width, height, palette_id, tiles_id, meta_id, flags, map_ptr, *_ = struct.unpack_from('<HH4B5I', rom, record)
        if not map_ptr or not width or not height:
            continue
        directory = resources / f'world-{scene:03d}'
        directory.mkdir(exist_ok=True)
        (directory / 'descriptor.bin').write_bytes(rom[record:record + 28])
        raw_resource(record, 28, 'scene descriptor', f'scene {scene}')
        tiles = bytearray(0x8000)
        tile_offsets = []
        for slot, dest in enumerate((0, 0x6000)):
            tile_id = rom[0x54E008 + tiles_id * 2 + slot]
            if tile_id:
                offset = u32(rom, 0x54DE90 + tile_id * 4) - ROM_BASE
                data = decoded(offset)
                if dest + len(data) > len(tiles):
                    raise ValueError(f'Scene {scene}: tiles overflow character memory')
                tiles[dest:dest + len(data)] = data
                tile_offsets.append(f'{offset:06x}')
        palette = bytearray(512)
        first, second = rom[0x54DDAC + palette_id * 2:0x54DDAC + palette_id * 2 + 2]
        palette_offsets = []
        if first:
            offset = u32(rom, 0x54DBB8 + first * 4) - ROM_BASE
            length = 320 if second else 384
            palette[128:128 + length] = rom[offset:offset + length]
            palette_offsets.append(f'{offset:06x}')
            raw_resource(offset, length, 'palette variant 0', f'scene {scene}; additional palette variants have unknown extent')
        if second:
            offset = u32(rom, 0x54DBB8 + second * 4) - ROM_BASE
            palette[448:512] = rom[offset:offset + 64]
            palette_offsets.append(f'{offset:06x}')
            raw_resource(offset, 64, 'palette variant 0 secondary', f'scene {scene}; additional palette variants have unknown extent')
        (directory / 'tiles.4bpp').write_bytes(tiles)
        (directory / 'palette.pal').write_bytes(palette)
        metatiles = u32(rom, 0x54E0C4 + meta_id * 4) - ROM_BASE
        raw_resource(0x54E0C4 + meta_id * 4, 4, 'metatile table pointer', f'scene {scene}')
        data = decoded(map_ptr - ROM_BASE)
        (directory / 'decoded-map.bin').write_bytes(data)
        layer_size = width * height // 2
        layers = []
        # Scene109 installs 0807d499 in its init0808b9c8: only first map is
        # passed to the metatile renderer. Later bytes are special scene data.
        layer_count = 1 if scene == 109 else min(2, len(data) // layer_size)
        metatile_offsets = set()
        for layer in range(layer_count):
            raw_map = data[layer * layer_size:(layer + 1) * layer_size]
            metatile_offsets.update(v for v, in struct.iter_unpack('<H', raw_map))
            (directory / f'layer-{layer}.metatiles').write_bytes(raw_map)
            tilemap = expand_metatiles(rom, metatiles, data[layer * layer_size:(layer + 1) * layer_size], width, height)
            (directory / f'layer-{layer}.map').write_bytes(tilemap)
            image = render_map(tiles, tilemap, palette, width)
            image.save(output / f'world-{scene:03d}-layer-{layer}.png')
            layers.append(image)
        first_relative = min(metatile_offsets)
        metatile_start = metatiles + first_relative
        metatile_size = max(metatile_offsets) + 8 - first_relative
        raw_resource(metatile_start, metatile_size, 'referenced metatile span',
                     f'scene {scene}; table base {metatiles:06x}; includes gaps between referenced entries')
        result = Image.alpha_composite(layers[1], layers[0]) if len(layers) == 2 else layers[0]
        save(f'world-{scene:03d}', result, ';'.join(tile_offsets), f'{map_ptr - ROM_BASE:06x}',
             ';'.join(palette_offsets),
             '0807eb64 scene directory; 0807d238 map/metatiles; 0807d462 palette/tiles; 08098552 metatile expansion; base palette variant 0',
             scene=scene, descriptor=f'{record:06x}', metatiles=f'{metatiles:06x}',
             metatile_range=f'{metatile_start:06x}+{metatile_size:x}', resources=f'resources/world-{scene:03d}',
             note=('Single world layer; scene init 0808b9c8 selects renderer 0807d499. ' if scene == 109 else '') + 'Base layers only; animated tile slots and event-dependent edits are not simulated')
    fields = ('name', 'preview', 'tiles', 'tilemap', 'palette', 'metatiles', 'scene', 'descriptor', 'metatile_range', 'resources', 'evidence', 'note')
    with (output / 'backgrounds.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    with (resources / 'sources.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=('file', 'offset', 'size', 'decoded_size', 'kind', 'evidence'))
        writer.writeheader()
        writer.writerows(sources.values())
    return rows


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rom', type=Path)
    parser.add_argument('--output', type=Path, default=Path('dist/graphics-full/backgrounds'))
    args = parser.parse_args()
    rows = export_backgrounds(args.rom.read_bytes(), args.output)
    print(f'{len(rows)} reconstructed backgrounds in {args.output}')
