#!/usr/bin/env python3
"""Reconstruct additional direct-loader scene graphics with traced palettes/maps."""
import _paths
import csv
import struct
from pathlib import Path
from PIL import Image
from slime_gfx import Decompressor, gba_bgr555_to_rgba


def export_scene_overlays(rom, output):
    from graphics_backgrounds import render_map, expand_metatiles
    output = Path(output)
    resources = output / 'resources'
    resources.mkdir(parents=True, exist_ok=True)
    rows, sources = [], []

    def raw(offset, size, kind):
        name = f'overlay-{offset:06x}-{size:x}.bin'
        data = rom[offset:offset + size]
        (resources / name).write_bytes(data)
        sources.append((name, f'{offset:06x}', size, kind))
        return data

    def unpack(offset):
        data, _, end = Decompressor(rom, offset).decompress()
        raw(offset, end - offset, 'packed stream')
        (resources / f'overlay-{offset:06x}.decoded').write_bytes(data)
        return data

    def palette(parts):
        data = bytearray(512)
        for offset, dest, size in parts:
            data[dest:dest + size] = raw(offset, size, 'palette')
        return data

    def save(name, image, tiles, tilemap, colors, tile_offsets, map_offsets, palette_offsets, evidence, note=''):
        name = 'scene-' + name
        directory = resources / name
        directory.mkdir(exist_ok=True)
        for suffix, data in (('tiles.bin', tiles), ('tilemap.bin', tilemap), ('palette.pal', colors)):
            (directory / suffix).write_bytes(data)
        image.save(output / f'{name}.png')
        rows.append(dict(name=name, preview=name + '.png',
                         tiles=';'.join(f'{v:06x}' for v in tile_offsets),
                         tilemap=';'.join(f'{v:06x}' for v in map_offsets),
                         palette=';'.join(f'{v:06x}' for v in palette_offsets),
                         evidence=evidence, note=note, resources=f'resources/{name}'))

    def text_bg(name, tile_offset, map_offset, palette_parts, evidence, *, tile_base=0, raw_map=None, width=32, crop=240):
        tiles = bytearray(32768)
        decoded = unpack(tile_offset)
        tiles[tile_base:tile_base + len(decoded)] = decoded
        tilemap = raw(map_offset, raw_map, 'tilemap') if raw_map else unpack(map_offset)
        colors = palette(palette_parts)
        image = render_map(tiles, tilemap, colors, width)
        if crop:
            image = image.crop((0, 0, min(crop, image.width), image.height))
        save(name, image, tiles, tilemap, colors, [tile_offset], [map_offset],
             [p[0] for p in palette_parts], evidence)
        return image

    # 08000b34 installs the common palette at color1; subarea labels replace its
    # character slots at0600f400, while 080957e0 copies each 32x3 map.
    for i in range(7):
        descriptor = 0x7117DC + i * 8
        tile, tilemap = (v - 0x08000000 for v in struct.unpack_from('<II', raw(descriptor, 8, 'label descriptor')))
        text_bg(f'subarea-label-{i:02d}', tile, tilemap, [(0x745A7E, 2, 0x5E)],
                '08095874 selects paired tile stream; 080957e0 copies 32x3 raw map; common palette08000b34',
                tile_base=0x7400, raw_map=192, crop=256)

    # Dialogue/history interface: palette colors1..31 are explicitly loaded.
    text_bg('dialogue-frame', 0x5639E0, 0x563F30, [(0x563EF2, 2, 62)],
            '0807f9b0 loads direct tiles/map and palette; runtime dialog is rendered separately')
    text_bg('dialogue-frame-label', 0x5639E0, 0x5680D4, [(0x563EF2, 2, 62)],
            '0807fa08 copies 8x3 tile rectangle into dialog frame', raw_map=48, width=8, crop=64)
    text_bg('credits-glyph-page', 0x564C38, 0x565734, [(0x74F5BE, 2, 30)],
            '0807fc24 DMA palette; 0807fd54 tile stream plus raw 30x10 page; other credit pages are generated',
            raw_map=600, width=30, crop=240)

    # These statue images use a three-entry pointer table, not three adjacent maps.
    raw(0x5580BC, 12, 'statue map pointers')
    for i in range(3):
        tilemap = struct.unpack_from('<I', rom, 0x5580BC + 4 * i)[0] - 0x08000000
        text_bg(f'statue-{i:02d}', 0x6A4EE0, tilemap, [(0x6A4EC0, 96, 32)],
                '080837ec loads tiles plus 05580bc[index] map and bank3 palette', crop=256)
    text_bg('lightning', 0x6A688C, 0x6A6FC0, [(0x6A6FA0, 96, 32)],
            '0808cc30 copies bank3 palette and decompresses tiles/map')

    def affine_image(name, tiles, tilemap, colors, width):
        # GBA affine BGs use one-byte maps and 64-byte 8bpp tiles.
        rgba = [gba_bgr555_to_rgba(c) for c, in struct.iter_unpack('<H', colors)]
        image = Image.new('RGBA', (width * 8, len(tilemap) // width * 8))
        for n, index in enumerate(tilemap):
            pixels = tiles[index * 64:(index + 1) * 64]
            if len(pixels) != 64:
                raise ValueError(f'{name}: affine tile index outside loaded tiles')
            tile = Image.new('RGBA', (8, 8))
            tile.putdata([rgba[v] if v else (0, 0, 0, 0) for v in pixels])
            image.paste(tile, (n % width * 8, n // width * 8))
        return image

    def affine(name, tile_offset, map_offset, palette_offset, evidence, raw_size=None, width=32):
        tiles = unpack(tile_offset)
        tilemap = raw(map_offset, raw_size, 'raw affine tilemap') if raw_size else unpack(map_offset)
        colors = palette([(palette_offset, 96, 32)])
        image = affine_image(name, tiles, tilemap, colors, width)
        save(name, image, tiles, tilemap, colors, [tile_offset], [map_offset], [palette_offset], evidence,
             'Affine BG source image; in-game rotation/scaling is runtime state')

    affine('affine-effect-8594c', 0x6A15AC, 0x6A1DD4, 0x6A1DB4,
           '0808594c sets affine BG2 and loads8bpp tiles/map plus bank3 palette')
    affine('affine-effect-8d8e4', 0x6A7E20, 0x6A8928, 0x6A8908,
           '0808d8e4 loads8bpp tiles/map plus bank3 palette')

    def world_resources(scene):
        descriptor = 0x54F198 + scene * 28
        record = raw(descriptor, 28, 'scene descriptor')
        palette_id, tile_id = record[4:6]
        tiles, colors = bytearray(32768), bytearray(512)
        tile_offsets, palette_offsets = [], []
        for slot, destination in enumerate((0, 0x6000)):
            index = rom[0x54E008 + tile_id * 2 + slot]
            if index:
                source = struct.unpack_from('<I', rom, 0x54DE90 + index * 4)[0] - 0x08000000
                decoded = unpack(source)
                tiles[destination:destination + len(decoded)] = decoded
                tile_offsets.append(source)
        first, second = rom[0x54DDAC + 2 * palette_id:0x54DDAC + 2 * palette_id + 2]
        for index, destination, length in ((first, 128, 320 if second else 384), (second, 448, 64)):
            if index:
                source = struct.unpack_from('<I', rom, 0x54DBB8 + index * 4)[0] - 0x08000000
                colors[destination:destination + length] = raw(source, length, 'world palette variant0')
                palette_offsets.append(source)
        return tiles, colors, tile_offsets, palette_offsets

    def screenblock_map(tilemap, width):
        if width == 32:
            return tilemap
        height = len(tilemap) // (width * 2)
        if height % 32 or width % 32:
            raise ValueError('Hardware screenblock dimensions must be multiples of32')
        linear = bytearray(len(tilemap))
        for y in range(height):
            for x in range(width):
                block = (y // 32) * (width // 32) + x // 32
                source = block * 2048 + ((y % 32) * 32 + x % 32) * 2
                destination = (y * width + x) * 2
                linear[destination:destination + 2] = tilemap[source:source + 2]
        return bytes(linear)

    def inherited(name, scene, map_offset, width, evidence, replacements=(), character_base=0):
        tiles, colors, tile_offsets, palette_offsets = world_resources(scene)
        if character_base:
            tiles = bytearray(32768)
        for offset, destination in replacements:
            data = unpack(offset)
            tiles[destination:destination + len(data)] = data
            tile_offsets.append(offset)
        tilemap = screenblock_map(unpack(map_offset), width)
        image = render_map(tiles, tilemap, colors, width)
        save(name, image, tiles, tilemap, colors, tile_offsets, [map_offset], palette_offsets, evidence,
             f'World scene{scene} palette variant0; hardware screenblocks rearranged to a linear editable map')

    inherited('restaurant-event-layer', 13, 0x595460, 64,
              'scene13 init08082cfc conditionally loads8192-byte map08082d38; BG0 d443 uses64x64 screenblocks')
    for scene in (18,):
        inherited(f'shop-{scene:03d}-event-layer', scene, 0x5C03E4, 64,
                  'scene17/18 shared init080830bc calls08083154 on event20; BG3 d440 uses64x64 screenblocks')
    for scene in (236, 239):
        for layer, offset in enumerate((0x56AE10, 0x56AF28)):
            inherited(f'intro-room-{scene}-layer-{layer}', scene, offset, 32,
                      'scene236/239 initializers080802e0/080803c4 load these maps; retained scene tiles and palettes')
    inherited('stadium-event-layer', 7, 0x5E2B40, 64,
              '08082774 supplies hardware map during stadium scene event; scene7 tiles/palette retained')
    inherited('house-event-layer', 8, 0x5A8330, 32,
              'scene8 init080828fc calls0808278c to load BG3 map; scene8 tiles/palette retained')
    inherited('tower-cloud-layer', 103, 0x6A4AA0, 64,
              'scene103 init0808a891 calls0808b0a0; new character block at8000 and64x32 BG3 map',
              replacements=((0x6A4148, 0),), character_base=0x8000)
    for layer, offset in enumerate((0x709B48, 0x709D30)):
        inherited(f'finale-layer-{layer}', 122, offset, 64,
                  'scene122 init0808d644 calls0808dac8 for two64x32 hardware maps; scene tiles/palette retained')
    # 08088930 explicitly installs these tiles and palette;0808868c reuses the map.
    tiles = unpack(0x6A1E84).ljust(32768, b'\0')
    tilemap = screenblock_map(unpack(0x6A2BA0), 64)
    colors = palette([(0x6A3098, 96, 32)])
    save('volcano-cloud-layer', render_map(tiles, tilemap, colors, 64), tiles, tilemap, colors,
         [0x6A1E84], [0x6A2BA0], [0x6A3098],
         '08088930 loads tiles/palette/map; BG3 d44a is64x64 hardware screenblocks')

    inherited('volcano-uniform-mask', 78, 0x6A2B94, 32,
              '08087d38 calls08088148; uniform32x20 map of tileb279; scene78 tiles/palette retained')
    inherited('volcano-event-wall', 89, 0x6A3274, 64,
              '08089066 loads tiles at9000;08089308 loads64x64 BG map; scene89 palette retained',
              replacements=((0x6A30B8, 0x1000),), character_base=0x8000)

    def world_layer(name, scene, layer, tile_override=None, palette_override=None, map_override=None):
        descriptor = raw(0x54F198 + scene * 28, 28, 'scene descriptor')
        width, height = struct.unpack_from('<HH', descriptor)
        map_offset = map_override or struct.unpack_from('<I', descriptor, 8)[0] - 0x08000000
        meta_pointer = 0x54E0C4 + descriptor[6] * 4
        raw(meta_pointer, 4, 'metatile pointer')
        base = struct.unpack_from('<I', rom, meta_pointer)[0] - 0x08000000
        data = unpack(map_offset)
        size = width * height // 2
        data = data[layer * size:(layer + 1) * size]
        offsets = [v for v, in struct.iter_unpack('<H', data)]
        raw(base + min(offsets), max(offsets) - min(offsets) + 8, 'referenced metatile table span')
        tilemap = expand_metatiles(rom, base, data, width, height)
        tiles, colors, tile_offsets, palette_offsets = world_resources(scene)
        if tile_override:
            tiles = unpack(tile_override).ljust(32768, b'\0')
            tile_offsets = [tile_override]
        if palette_override:
            colors[96:128] = raw(palette_override, 32, 'effect palette bank3')
            palette_offsets.append(palette_override)
        save(name, render_map(tiles, tilemap, colors, width), tiles, tilemap, colors,
             tile_offsets, [map_offset], palette_offsets,
             f'Scene{scene} map layer{layer}; ROM metatile expander08098552; direct tile override when specified')

    for scene, offset in ((5, 0x5E21C0), (6, 0x5E34E0)):
        for layer in range(2):
            world_layer(f'world-{scene:03d}-event-layer-{layer}', scene, layer, map_override=offset)
    world_layer('world-004-wave-layer', 4, 0, tile_override=0x5CF154)
    world_layer('world-113-distortion-layer', 113, 1, tile_override=0x6A3AFC, palette_override=0x6A3ADC)

    # Third metatile planes are selected by alternate scene renderers.
    for scene in (0, 3, 4, 9, 15, 33, 34, 35, 36, 37, 38, 39, 40, 73, 78, 87, 88, 105, 107, 108, 113, 120, 121, 150, 178, 182):
        world_layer(f'world-{scene:03d}-third-layer', scene, 2)
    world_layer('world-038-event-layer', 38, 0, map_override=0x5DDF50)

    # Five explicit8x8 affine frames, selected by0808cd50 and called with0..4.
    for frame in range(5):
        affine(f'affine-face-frame-{frame}', 0x6A7398, 0x6A7CE0 + frame * 64, 0x6A7CC0,
               '0808cc8c loads8bpp tiles/palette;0808cd50 copies frame*64 as8x8 byte map', raw_size=64, width=8)

    # Scene109's remaining map bytes are an affine byte map, not metatile indices.
    descriptor = raw(0x54F198 + 109 * 28, 28, 'scene descriptor')
    width, height = struct.unpack_from('<HH', descriptor)
    map_offset = struct.unpack_from('<I', descriptor, 8)[0] - 0x08000000
    tilemap = unpack(map_offset)[width * height // 2:]
    tiles = unpack(0x6A33B4)
    colors = palette([(0x6A3394, 96, 32)])
    save('world-109-affine-layer', affine_image('world109', tiles, tilemap, colors, width),
         tiles, tilemap, colors, [0x6A33B4], [map_offset], [0x6A3394],
         '0808ba6c passes scene map+18 to08095028;080985e4 copies byte rows;08094f5c selects8bpp BG2',
         'Full affine source map; in-game viewport/scale is runtime state')

    descriptor = raw(0x54F198 + 118 * 28, 28, 'scene descriptor')
    width, height = struct.unpack_from('<HH', descriptor)
    map_offset = struct.unpack_from('<I', descriptor, 8)[0] - 0x08000000
    tilemap = unpack(map_offset)[width * height:]
    save('world-118-affine-layer', affine_image('world118', tiles, tilemap, colors, width),
         tiles, tilemap, colors, [0x6A33B4], [map_offset], [0x6A3394],
         '0808cff0 passes scene map+1c to08095028;080985e4 copies byte rows;08094f5c selects8bpp BG2',
         'Full affine source map; in-game viewport/scale is runtime state')

    # Four14x12 intro animation rectangles (080804b0 computes frame*336).
    tiles, colors, tile_offsets, palette_offsets = world_resources(236)
    for frame in range(4):
        offset = 0x56B024 + frame * 336
        tilemap = raw(offset, 336, 'raw intro animation rectangle14x12')
        save(f'intro-character-frame-{frame}', render_map(tiles, tilemap, colors, 14), tiles, tilemap, colors,
             tile_offsets, [offset], palette_offsets, '080804b0 copies14x12 tile rectangle; intro scene236 character bank/palette')

    # Six arrow-animation steps, with separate left/right2x2 rectangles.
    for frame in range(6):
        for side in range(2):
            text_bg(f'dialogue-arrow-{frame}-{side}', 0x5639E0, 0x568074 + frame * 16 + side * 8,
                    [(0x563EF2, 2, 62)], '0807fb74 selects frame*16; two2x2 map rectangles',
                    raw_map=8, width=2, crop=16)

    # Scene68 installs a raw20x42 rectangle in a32x64 BG at row18.
    _, colors, _, palette_offsets = world_resources(68)
    tiles = unpack(0x6495AC).ljust(32768, b'\0')
    tilemap = raw(0x6A2504, 20 * 42 * 2, 'raw tile rectangle20x42')
    save('water-column', render_map(tiles, tilemap, colors, 20), tiles, tilemap, colors,
         [0x6495AC], [0x6A2504], palette_offsets,
         '08086d68 loads character block;08086d80 copies20x42 raw rectangle; scene68 palette variant0')

    # Arena team labels use the six-tile2-row formula in08094ba8.
    _, colors, _, palette_offsets = world_resources(22)
    tiles = unpack(0x6035D4).ljust(32768, b'\0')
    for team in range(4):
        tilemap = struct.pack('<6H', *(0x5050 + team * 6 + n for n in range(6)))
        save(f'arena-team-label-{team}', render_map(tiles, tilemap, colors, 3), tiles, tilemap, colors,
             [0x6035D4], [], palette_offsets,
             '08093f8c loads6035d4;08094ba8 builds3x2 labels from tile5050+team*6; arena scene22 palette',
             'Generated map follows ROM builder; no stored map pointer')

    # Large arena digits are8bpp and use a different font from the small HUD.
    # 08094568 writes a3x4 map at tile65+12*glyph;0..9 plus the separator.
    tiles = bytearray(0x1040) + unpack(0x603A74)
    colors = palette([(0x603574, 128, 32)])
    for glyph in range(11):
        tilemap = bytes(range(65 + glyph * 12, 65 + (glyph + 1) * 12))
        save(f'arena-large-digit-{glyph:02d}', affine_image('arena digit', tiles, tilemap, colors, 3),
             tiles, tilemap, colors, [0x603A74], [], [0x603574],
             '0809407a loads8bpp tiles at9040;08094568 constructs3x4 glyph;08094aa8 selects bank4 palette',
             'Palette variant0; generated numeric map follows ROM builder')

    # The six intro maps are staged before their tile/palette load.080d3b60
    # supplies archive1b6 tiles and1b7 palette for BG2/BG3 character block0.
    tiles = unpack(0x796A3C).ljust(32768, b'\0')
    colors = bytearray(512)
    intro_colors = unpack(0x79A41C)
    colors[32:32 + len(intro_colors)] = intro_colors
    for i, offset in enumerate((0x7676B4, 0x767D58, 0x768028, 0x7683AC, 0x768A40, 0x768D2C)):
        tilemap = unpack(offset)
        save(f'intro-scroll-map-{i}', render_map(tiles, tilemap, colors, 32), tiles, tilemap, colors,
             [0x796A3C], [offset], [0x79A41C],
             '080d3700 stages maps0..5;080d3b60 later loads archive1b6 tiles/1b7 palette;080d3c50 sets BG2/BG3')

    # Intro scenery loaded from UI archive; verified by08080200, including its
    # compressed 255-color palette starting at hardware color1.
    tiles = unpack(0x76AC40).ljust(32768, b'\0')
    colors = bytearray(512)
    palette_data = unpack(0x76C368)
    colors[2:2 + len(palette_data)] = palette_data
    for i, offset in enumerate((0x76C61C, 0x76C508, 0x76F3A4)):
        tilemap = unpack(offset)
        image = render_map(tiles, tilemap, colors, 32)
        save(f'intro-layer-{i}', image, tiles, tilemap, colors, [0x76AC40], [offset], [0x76C368],
             '08080200 archive entries08f/090/093/092/098; decompressed palette255 colors at05000002',
             'Separate source layer; in-game scrolling and composition are runtime state')

    with (resources / 'scene-overlay-sources.csv').open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(('file', 'offset', 'size', 'kind'))
        writer.writerows(dict.fromkeys(sources))
    with (output / 'scene-overlays.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=('name', 'preview', 'tiles', 'tilemap', 'palette', 'evidence', 'note', 'resources'))
        writer.writeheader()
        writer.writerows(rows)
    return rows


if __name__ == '__main__':
    import argparse
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('rom', type=Path)
    p.add_argument('--output', type=Path, default=Path('dist/graphics-full/backgrounds'))
    args = p.parse_args()
    print(len(export_scene_overlays(args.rom.read_bytes(), args.output)), 'scene overlays exported')
