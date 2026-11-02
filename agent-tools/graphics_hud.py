"""Export HUD rectangles using the tile/map bindings in the original loaders."""
import struct
from pathlib import Path

from PIL import Image

from graphics_backgrounds import render_map
from slime_gfx import Decompressor


def export_hud(rom, output):
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    # 08000B34: 47 palette halfwords -> 05000002, tiles -> 0600F400.
    # These maps address character base 06008000, so the resident tiles start
    # at relative byte 7400. Palette index zero is transparent.
    resident = bytearray(0x8000)
    data = Decompressor(rom, 0x745AFC).decompress()[0]
    resident[0x7400:0x7400 + len(data)] = data
    base_palette = b'\0\0' + rom[0x745A7E:0x745ADC]
    rows = []

    def emit(name, tiles, palette, map_offset, width, height, count,
             tile_source, palette_source, evidence, note):
        size = width * height * 2
        image = Image.new('RGBA', (width * 8 * count, height * 8))
        for i in range(count):
            tilemap = rom[map_offset + i * size:map_offset + (i + 1) * size]
            frame = render_map(tiles, tilemap, palette, width)
            image.alpha_composite(frame, (i * width * 8, 0))
        filename = f'hud-{name}.png'
        image.save(output / filename)
        rows.append(dict(name=f'hud/{name}', preview=filename, tiles=tile_source,
                         tilemap=f'{map_offset:06x}', palette=palette_source,
                         evidence=evidence, note=note))

    # 0809B020 selects a 9x2 rectangle at base[maximum] + current*36.
    # The seven contiguous arrays contain 9,11,...21 possible fill states.
    starts = [p - 0x08000000 for p in struct.unpack_from('<7I', rom, 0x7408C0)]
    for i, start in enumerate(starts):
        emit(f'hearts-{8 + i * 2}', resident, base_palette, start, 9, 2,
             9 + i * 2, '745afc', '745a7e',
             '08000b34..08000b4e; 0809b020..0809b046; table 087408c0',
             'Every fill state, left to right; heart icons only, no Japanese lettering')

    # 0809A834 selects tiles and a 15-color palette from table 087408DC.
    # Tiles go to F040; 080995EC copies palette colors 1..15 into bank 2.
    # The meter renderer chooses one of fifteen 2x2 maps at 74D474.
    for kind in range(1, 9):
        tiles_offset, palette_offset = struct.unpack_from('<II', rom, 0x7408DC + kind * 8)
        tiles_offset -= 0x08000000
        palette_offset -= 0x08000000
        tiles = bytearray(resident)
        data = Decompressor(rom, tiles_offset).decompress()[0]
        tiles[0x7040:0x7040 + len(data)] = data
        palette = bytearray(base_palette)
        palette[66:96] = rom[palette_offset:palette_offset + 30]
        emit(f'meter-{kind}', tiles, palette, 0x74D474, 2, 2, 15,
             f'{tiles_offset:06x}', f'{palette_offset:06x}',
             '0809a834..0809a85a; 0809a938..0809a944; table 087408dc',
             'All fifteen icon fill states; no Japanese lettering')
        if kind == 3:
            # The multi-stage meter swaps 30 raw tiles as its stage changes.
            for offset in (0x74DCC4, 0x74E084):
                alternate = bytearray(tiles)
                alternate[0x7040:0x7400] = rom[offset:offset + 0x3C0]
                emit(f'meter-3-{offset:06x}', alternate, palette, 0x74D474,
                     2, 2, 15, f'{offset:06x}', f'{palette_offset:06x}',
                     '0809a8fc..0809a90c',
                     'Runtime meter stage, all fifteen fill states; no Japanese lettering')
        if kind == 8:
            data = Decompressor(rom, 0x74F468).decompress()[0]
            tiles[0x6C40:0x6C40 + len(data)] = data
            emit('meter-8-secondary', tiles, palette, 0x74F714, 2, 2, 15,
                 '74f468', f'{palette_offset:06x}',
                 '0809a864..0809a868; 0809aacc..0809aad8',
                 'All fifteen secondary icon fill states; no Japanese lettering')

    for name, offset in [('mail-small', 0x74E918), ('mail-large', 0x74E904)]:
        emit(name, resident, base_palette, offset, 3, 3, 1, '745afc', '745a7e',
             '08000b34..08000b4e; 0809b060..0809b0c0',
             'Mail notification icon; no Japanese lettering')
    return rows
