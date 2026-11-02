"""Additional UI tilemaps, including rectangles copied without archive decoding."""
import struct
from pathlib import Path

from PIL import Image
from slime_gfx import gba_bgr555_to_rgba

from graphics_backgrounds import render_map
from slime_gfx import Decompressor


def export_extra_ui(rom, output, entries):
    output = Path(output)
    rows = []

    def stream(offset):
        return Decompressor(rom, offset).decompress()[0]

    def get(index):
        p, n = entries[index]
        data = rom[p:p+n]
        return stream(p) if data[:1] == b'\x70' else data

    def emit(name, tiles, palette, maps, evidence, tile_sources, palette_sources, affine=False):
        for label, offset, data, width in maps:
            filename = f'{name}-{label}.png'
            image = render_affine(tiles, data, palette, width) if affine else render_map(tiles, data, palette, width)
            image.save(output / filename)
            # Store the assembled input too: these images can be reconstructed
            # without guessing a width, palette shift or VRAM tile offset.
            resource = output / 'resources' / f'{name}-{label}'
            resource.mkdir(parents=True, exist_ok=True)
            for suffix, content in (('tiles.4bpp', tiles), ('palette.pal', palette), ('layer.map', data)):
                (resource / suffix).write_bytes(content)
            rows.append(dict(name=f'{name}/{label}', preview=filename,
                             tiles=tile_sources, tilemap=f'{offset:06x}', palette=palette_sources,
                             evidence=evidence, note=f'{"Affine 8bpp" if affine else "4bpp"} static layer; {width} columns; runtime text/overlays omitted'))

    def archive_maps(name, tid, pid, ids, evidence, pal_start=2, tile_start=0, extra_tiles=(), map_or=0):
        tiles = bytearray(32768)
        data = get(tid)
        tiles[tile_start:tile_start+len(data)] = data
        for index, start in extra_tiles:
            data = get(index)
            tiles[start:start+len(data)] = data
        palette = bytearray(512)
        data = get(pid)
        palette[pal_start:pal_start+len(data)] = data
        maps = [(f'{i:03x}', entries[i][0],
                 b''.join(struct.pack('<H',v|map_or if v else 0) for v, in struct.iter_unpack('<H',get(i))),w) for i,w in ids]
        emit(name, tiles, palette, maps, evidence,
             ';'.join(f'{entries[i][0]:06x}' for i in (tid, *(x[0] for x in extra_tiles))),
             f'{entries[pid][0]:06x}')

    # Menu navigation uses different maps over the same tile bank.
    archive_maps('main-menu-extra', 0xDF, 0xE0, [(i,32) for i in (0xD2,0xD5,0xD6,0xD7)],
                 '080c7540..080c7666; 080c78c4; 080c8ec4')
    # Nine page types, table-selected tiles/palettes and page-map lists.
    # 764EB0 holds pointers into the short lists preceding it; 764ED4 is
    # the companion map table. Each list ends at the next distinct pointer.
    tile_ids = struct.unpack_from('<9H',rom,0x764E9C)
    pal_ids = struct.unpack_from('<9H',rom,0x764EE6)
    pointers = [x-0x08000000 for x in struct.unpack_from('<9I',rom,0x764EB0)]
    for page,(tid,pid,pointer) in enumerate(zip(tile_ids,pal_ids,pointers)):
        end = min([p for p in pointers if p>pointer]+[0x764E9C])
        ids = list(struct.unpack_from('<'+'H'*((end-pointer)//2),rom,pointer))
        ids += [struct.unpack_from('<H',rom,0x764ED4+2*page)[0]]
        archive_maps(f'menu-page-{page}',tid,pid,[(i,32) for i in ids],
                     '080c8aa6..080c8ad8; tables 08764e9c/08764ee6/08764eb0/08764ed4')
    archive_maps('ranking-extra',0x132,0x133,[(i,32) for i in (0x135,0x137,0x138)],
                 '080c29cc..080c29f4')
    archive_maps('ranking-labels',0x132,0x133,[(i,7) for i in range(0x151,0x163)],
                 '080c2e82/080c2eb2 and 080c3158/080c318a: 7x2 label rectangles')
    archive_maps('rescue-extra',0x169,0x16B,[(0x172,32)],
                 '080bb9b2')
    archive_maps('status-menu',0x08F,0x090,[(i,32) for i in (0x091,0x092,0x093,0x098,0x09A)],
                 '0808020e..08080252; 080b75fc..080b7680')
    archive_maps('status-result',0x095,0x090,[(0x094,32)],'080a8342..080a837a')
    archive_maps('minigame-field',0x0C4,0x0C5,[(i,32) for i in (0x0C9,0x0CC,0x0CD)],
                 '080a9768..080a97a2; 080ab1ac..080ab1d0',pal_start=0x60)
    archive_maps('minigame-results',0x0C7,0x0C5,[(0x0C8,32)],
                 '080a93fa..080a9408',pal_start=0x60)
    archive_maps('minigame-menu',0x1B1,0x1B5,[(0x1B3,32),(0x1B4,32)],
                 '080be9ac..080be9da',pal_start=0)
    archive_maps('minigame-menu-revealed',0x1B1,0x1B2,[(0x1B3,32),(0x1B4,32)],
                 '080bf042: alternate palette decoded into shadow palette 0200f370',pal_start=0)
    archive_maps('title-bg',0x19F,0x1A0,[(0x1A1,32)],'080d443c..080d445a',pal_start=0)
    archive_maps('title-overlay',0x1A2,0x1A4,[(0x1A3,32)],'080d3c82..080d3c90',pal_start=0)
    archive_maps('intro-bg',0x1B6,0x1B7,[(0x1B8,32)],'080d3b76..080d3b98',pal_start=32)
    archive_maps('pot-panels',0x1BA,0x1BB,[(i,w) for i,w in
                 ((0x1BC,32),(0x1BD,16),(0x1BE,16),(0x1BF,32),(0x1C0,32),
                  (0x1C1,8),(0x1C2,8),(0x1C3,8),(0x1C4,8),(0x1C5,16))],
                 '080c5980..080c599c; 080c5bc2; 080c5c9e; 080c616a; 080c622a',pal_start=0)
    archive_maps('town-extra',0x1C6,0x1C7,[(i,w) for i,w in
                 [(0x1CE,32),(0x1D7,32),(0x1DF,14),(0x1E0,14),(0x1E1,14),(0x1E2,14),(0x1E3,12),
                  (0x1F1,32)]+[(i,12) for i in range(0x1E7,0x1EF)]+[(i,12) for i in range(0x1D8,0x1DF)]],
                 '080c72cc..080c72f2; 080c783c; 080cbc48..080cbc5e; 080d0100; 080d09c4..080d0cde')
    for tid,pid,mid,start in ((0x1F2,0x1F3,0x1F4,0x180),(0x1F8,0x1F9,0x1FA,0x160),
                               (0x1FD,0x1FE,0x1FF,0x180),(0x202,0x203,0x204,0x180),
                               (0x205,0x206,0x207,0x180),(0x208,0x206,0x209,0x180),
                               (0x20A,0x20B,0x20C,0x160)):
        archive_maps('town-building',tid,pid,[(mid,32)],'080d0924..080d0cc6',
                     pal_start=start,tile_start=0x6000,map_or=0x300)
    archive_maps('main-menu-labels',0xDF,0xE0,
                 [(i,w) for i,w in [(0xE1,5)]+[(i,2) for i in range(0xE2,0xEA)]+
                  [(i,4) for i in range(0xEA,0xEE)]+[(i,2) for i in range(0xEF,0xF9)]+
                  [(0x101,2),(0x102,2)]+[(i,12) for i in range(0x103,0x10B)]],
                 '080c8048..080c8220; 080c8626..080c86e0: menu indicator rectangles')
    archive_maps('rescue-labels',0x169,0x16B,
                 [(i,w) for i,w in [(0x173,10)]+[(i,5) for i in range(0x174,0x17A)]+
                  [(0x17A,11)]+[(i,4) for i in range(0x17B,0x181)]+[(i,2) for i in range(0x181,0x187)]],
                 '080bcbd2/080bcc32; 080bceb6/080bcf12: counter and indicator rectangles')
    archive_maps('status-labels',0x95,0x90,[(i,w) for i,w in
                 [(0x18A,2)]+[(i,6) for i in (0x18B,0x18C,0x18E,0x18F,0x190)]+
                  [(0x18D,4)]],
                 '080a83ec..080a8470; 080a8a0c..080a8c7c; 080bd566..080bd756')
    archive_maps('status-indicators',0x8F,0x90,[(i,2) for i in (0x197,0x198,0x19B,0x19C)],
                 '080b7616; 080b7ce2..080b7daa; 080bd566..080bd756')
    archive_maps('rescue-indicators',0x169,0x16B,[(i,2) for i in (0x191,0x192,0x193,0x194,0x195,0x196,0x199,0x19A,0x19D,0x19E)],
                 '080bd566..080bd5e4; 080bd734..080bd756; shared rescue tile bank')
    archive_maps('ranking-numbers',0x132,0x133,[(i,1) for i in range(0x13B,0x151)]+[(i,2) for i in range(0x163,0x169)],
                 '080c2456..080c2974; 080c2a60..080c3392: dynamic numeric rectangles')
    archive_maps('town-labels',0x1C6,0x1C7,[(i,w) for i,w in
                 [(i,2) for i in range(0x1C8,0x1CE)]+[(i,4) for i in range(0x1CF,0x1D3)]+
                  [(i,2) for i in range(0x1D3,0x1D7)]+[(0x1E5,1),(0x1E6,2)]+
                  [(0x20D,1),(0x20E,2),(0x20F,4),(0x210,3),(0x211,4),(0x212,1),
                   (0x213,2),(0x216,3)]],
                 '080ca894..080ca96c; 080cac82..080cadc4')
    emit('intro-logo',get(0x187).ljust(0x4000,b'\0'),get(0x188)+bytes(480),
         [('189',entries[0x189][0],get(0x189)[:1024],32)],
         '080d37d2..080d37ee; affine BG2 control5843; visible 32x32 map',
         f'{entries[0x187][0]:06x}',f'{entries[0x188][0]:06x}',affine=True)
    tiles=bytearray(32768)
    data=get(0xB4);tiles[0x7C00:0x7C00+len(data)]=data
    palette=get(0xB5)+bytes(480)
    emit('shop-frames',tiles,palette,
         [(f'{i:03x}',entries[i][0],get(i),w) for i,w in
          ((0xAB,9),(0xAC,9),(0xAD,19),(0xAE,19),(0xAF,28),(0xB0,28),(0xB1,9),(0xB2,9),(0xB3,9))],
         '08007822..08007854; 08007ccc/080083ec; 0800c030/0800e14a; 080a7c0e',
         f'{entries[0xB4][0]:06x}',f'{entries[0xB5][0]:06x}')
    palette=bytes(2)+rom[0x745A7E:0x745ADC]+bytes(416)
    for name,tid,mid,evidence in (
            ('overlay-window',0xA9,0xAA,'080c3cf4..080c3d0a; common palette bank1'),
            ('minigame-counter-frame',0x1F,0x20,'080c21aa..080c21c0; common palette')):
        emit(name,get(tid).ljust(32768,b'\0'),palette,
             [(f'{mid:03x}',entries[mid][0],get(mid),32)],evidence,
             f'{entries[tid][0]:06x}','745a7e')
    # File menu raw rectangles, including the empty-save panel.
    tiles=stream(0x751B90);palette=bytes(128)+stream(0x751AB0)
    specs=[(0x756140,28,7),(0x7576F0,4,4),(0x757770,2,2),(0x757778,4,2)]
    specs += [(p,3,4) for p in range(0x757710,0x757770,24)]
    specs += [(p,2,2) for p in range(0x757808,0x757828,8)]
    emit('file-menu-extra',tiles,palette,
         [(f'{p:06x}',p,rom[p:p+w*h*2],w) for p,w,h in specs],
         '080d66b6/080d66d6; 080d75a4; 080d7aa2; 080d7c78..080d7e16; 080d8668',
         '751b90','751ab0')
    # Name-entry character grids: indexed pairs of compressed tiles and maps, not archive IDs.
    palette=bytes(128)+stream(0x755AD8)
    for i in range(3):
        t,m=[p-0x08000000 for p in struct.unpack_from('<II',rom,0x7656E4+i*8)]
        emit('name-entry',stream(t),palette,[(str(i),m,stream(m),32)],
             '080d1e68; 080d1ece..080d1ed6; 080d2bf4..080d2c10; table 087656e4',f'{t:06x}','755ad8',affine=True)
    tiles=bytearray(32768)
    data=stream(0x74F90C);tiles[:len(data)]=data
    emit('name-entry-frame',tiles,palette,
         [('panel',0x74FE74,stream(0x74FE74),32),('background',0x74FE54,stream(0x74FE54),32)],
         '080d1e72..080d1e7e; 080d3584..080d358a','74f90c','755ad8')
    # Startup/legal overlay and its three table-selected variants.
    palette=bytes(0x100)+rom[0x7556C4:0x755704]+bytes(0xC0)
    emit('startup',stream(0x755B94),palette,[('frame',0x755F54,stream(0x755F54),32)],
         '080d561a..080d563c','755b94','7556c4')
    for i in range(3):
        t,m=[p-0x08000000 for p in struct.unpack_from('<II',rom,0x765B90+i*8)]
        emit('startup',stream(t),palette,[(str(i),m,stream(m),16)],
             '080d5640..080d565e; table 08765b90',f'{t:06x}','7556c4',affine=True)
    return rows


def render_affine(tiles, tilemap, palette, width):
    """GBA affine BG: byte tile indices, 8bpp tiles, no flips or palette banks."""
    assert len(tilemap) % width == 0
    colors=[gba_bgr555_to_rgba(v) for v, in struct.iter_unpack('<H',palette)]
    result=Image.new('RGBA',(width*8,len(tilemap)//width*8))
    for i,t in enumerate(tilemap):
        pixels=tiles[t*64:t*64+64]
        if len(pixels)!=64:
            raise ValueError(f'Affine tile {t} outside supplied bank')
        tile=Image.new('RGBA',(8,8))
        tile.putdata([colors[v] if v else (0,0,0,0) for v in pixels])
        result.paste(tile,(i%width*8,i//width*8))
    return result
