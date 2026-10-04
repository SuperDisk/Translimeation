#!/usr/bin/env python3
"""Execute the original Thumb glyph renderer and check its width contract.

Optional test dependency: unicorn. No full-system/gameplay emulation is claimed.
DMA3 copies/fills are simulated so the returned advance AND composed pixels can
be compared with the packed 2bpp font data. Run from the repository root.
"""
import _paths

import struct
from pathlib import Path
from unicorn import Uc, UC_ARCH_ARM, UC_MODE_THUMB, UC_HOOK_MEM_WRITE
from unicorn.arm_const import (UC_ARM_REG_R0, UC_ARM_REG_R1, UC_ARM_REG_R2,
                              UC_ARM_REG_R3, UC_ARM_REG_SP, UC_ARM_REG_LR, UC_ARM_REG_PC)

ROM = Path('slime_original.gba').read_bytes()
ENDS = (0x13, 0x18, 0x23, 0x3D, 0x66, 0x97, 0xCE, 0x10C, 0x141, 0x1B8)
CONTEXT = 0x02001080
DEST = 0x06008000


def create_cpu():
    cpu = Uc(UC_ARCH_ARM, UC_MODE_THUMB)
    for address, size in [(0x08000000, 0x800000), (0x02000000, 0x40000),
                          (0x03000000, 0x8000), (0x06000000, 0x20000), (0x04000000, 0x1000)]:
        cpu.mem_map(address, size)
    cpu.mem_write(0x08000000, ROM)

    def dma(cpu, access, address, size, value, data):
        if address != 0x040000DC or not value & (1 << 31):
            return
        source, dest = struct.unpack('<II', cpu.mem_read(0x040000D4, 8))
        unit = 4 if value & (1 << 26) else 2
        count = (value & 0xFFFF) or 0x10000
        source_mode, dest_mode = (value >> 23) & 3, (value >> 21) & 3
        assert source_mode in (0, 2) and dest_mode == 0
        payload = bytes(cpu.mem_read(source, unit if source_mode == 2 else unit * count))
        cpu.mem_write(dest, payload * count if source_mode == 2 else payload)
    cpu.hook_add(UC_HOOK_MEM_WRITE, dma, begin=0x040000DC, end=0x040000DC)
    return cpu


def draw(cpu, code, cursor, color=0):
    for reg, value in [(UC_ARM_REG_R0, CONTEXT), (UC_ARM_REG_R1, code),
                       (UC_ARM_REG_R2, cursor), (UC_ARM_REG_R3, color),
                       (UC_ARM_REG_SP, 0x03007000), (UC_ARM_REG_LR, 0x03000001)]:
        cpu.reg_write(reg, value)
    cpu.emu_start(0x08096AA9, 0x03000000, count=200000)
    assert cpu.reg_read(UC_ARM_REG_PC) == 0x03000000, 'Renderer did not return'
    return cpu.reg_read(UC_ARM_REG_R0)


def reset(cpu):
    cpu.mem_write(CONTEXT, b'\x11' * 256 + struct.pack('<I', DEST) + b'\0' * 252)
    cpu.mem_write(DEST, b'\x11' * 4096)


def descriptor(code):
    group = next(n for n, end in enumerate(ENDS) if code <= end)
    return struct.unpack_from('<IHBB', ROM, 0x713EB8 + group * 8)


def glyph_pixels(code, color):
    pointer, start, width, stride = descriptor(code)
    pos = pointer - 0x08000000 + (code - start) * stride
    pixels = []
    for value in ROM[pos:pos + stride]:
        for shift in (6, 4, 2, 0):
            shade = (value >> shift) & 3
            pixels.append(1 if shade == 0 else shade + 1 + color * 3)
    return [pixels[y * width:(y + 1) * width] for y in range(16)]


def main():
    cpu = create_cpu()
    checks = 0
    for code in range(0x10, 0x1B9):
        for cursor in (0, 1, 207):
            reset(cpu)
            expected = descriptor(code)[2] + bool(cursor)
            assert draw(cpu, code, cursor) == expected, hex(code)
            checks += 1
    # Check every tile-alignment case, color, and font width with the real blitter.
    for leading in range(8):
        for code in (0x10, 0x14, 0x19, 0x24, 0x3E, 0x67, 0x98, 0xCF, 0x10D, 0x142):
            for color in range(4):
                reset(cpu)
                cursor = 0
                expected = [[] for _ in range(16)]
                for glyph in [0x10] * leading + [code]:
                    for y, row in enumerate(glyph_pixels(glyph, color)):
                        expected[y] += ([1] if cursor else []) + row
                    cursor += draw(cpu, glyph, cursor, color)
                data = cpu.mem_read(DEST, ((cursor + 7) // 8) * 64)
                for y in range(16):
                    actual = []
                    for x in range(cursor):
                        value = data[(x // 8) * 64 + y * 4 + (x % 8) // 2]
                        actual.append((value >> ((x & 1) * 4)) & 15)
                    assert actual == expected[y], (leading, hex(code), color, y)
                checks += 1
    # COLOR >=4 consumes the same width but draws blank palette-1 pixels.
    for color in (4, 255):
        for code in ENDS:
            for cursor in (0, 1):
                reset(cpu)
                advance = descriptor(code)[2] + bool(cursor)
                assert draw(cpu, code, cursor, color) == advance
                size = ((advance + 7) // 8) * 64
                assert cpu.mem_read(DEST, size) == b'\x11' * size
                checks += 1
    print(f'{checks} original-ROM renderer checks passed (advances and composed pixels).')


if __name__ == '__main__':
    main()
