#!/usr/bin/env python3
"""Run the original dialogue interpreter, with UI/audio boundary calls recorded.

Requires unicorn. The dispatcher, continuations, PC/argument handling, input
checks, actor-busy gate, numeric substitution, clear and window toggle execute
from the original ROM. Stubbed boundaries are explicit below; this is not a
full-system timing/playtest claim.
"""
import struct
from unicorn import UC_HOOK_CODE
from unicorn.arm_const import *
from probe_text_renderer import create_cpu, CONTEXT as C, DEST

SCRIPT = 0x02030000
REGS = (UC_ARM_REG_R0, UC_ARM_REG_R1, UC_ARM_REG_R2, UC_ARM_REG_R3)


class Interpreter:
    def __init__(self):
        self.cpu = create_cpu()
        self.calls = []
        self.flag = 0
        # Keep the interpreter and actor-busy predicate real. Intercept only
        # dialogue glyph output, UI creation/drawing and audio. Scroll and
        # speaker-name copies execute for real, including simulated DMA.
        self.stubs = {0x80969B8: 'glyph', 0x8097D6C: 'name-label',
                      0x80964A0: 'scroll-step', 0x80972AC: 'menu',
                      0x8097CF0: 'prompt', 0x8097C60: 'hide-window',
                      0x8097C08: 'show-window', 0x8002BAC: 'sound',
                      0x8070448: 'game-flag'}
        self.cpu.hook_add(UC_HOOK_CODE, self.hook)

    def hook(self, cpu, address, size, data):
        if address not in self.stubs:
            return
        name = self.stubs[address]
        args = tuple(cpu.reg_read(r) for r in REGS)
        self.calls.append((name, args))
        if name == 'scroll-step':
            return  # observer only: run the original scroll/DMA code
        cpu.reg_write(UC_ARM_REG_R0, self.flag if name == 'game-flag' else 0)
        cpu.reg_write(UC_ARM_REG_PC, cpu.reg_read(UC_ARM_REG_LR))

    def write(self, offset, value, size=1):
        self.cpu.mem_write(C + offset, value.to_bytes(size, 'little'))

    def u8(self, offset):
        return self.cpu.mem_read(C + offset, 1)[0]

    def u32(self, offset):
        return int.from_bytes(self.cpu.mem_read(C + offset, 4), 'little')

    def reset(self, data):
        self.calls.clear()
        self.cpu.mem_write(C, bytes(0x200))
        self.cpu.mem_write(SCRIPT, bytes(data) + b'\0' * 16)
        for offset, value, size in [(0x100, DEST, 4), (0x108, DEST, 4),
                                    (0x10C, SCRIPT, 4), (0x110, 0x02021000, 4), (0x126, 1, 1),
                                    (0x129, 1, 1), (0x12A, 1, 1),
                                    (0x12B, 26, 1), (0x12C, 4, 1)]:
            self.write(offset, value, size)
        self.cpu.mem_write(0x03002F40, struct.pack('<I', 0x02020000))
        self.cpu.mem_write(0x02020000, bytes(32))
        self.cpu.mem_write(0x03004260, bytes(21 * 32))
        self.keys(0)
        self.flag = 0

    def keys(self, value):
        self.cpu.mem_write(0x03004054, struct.pack('<H', value))

    def step(self, clear_delay=True):
        if clear_delay:
            self.write(0x128, 0)
        self.cpu.reg_write(UC_ARM_REG_SP, 0x03007000)
        self.cpu.reg_write(UC_ARM_REG_LR, 0x03000001)
        self.cpu.emu_start(0x08095F35, 0x03000000, count=100000)
        assert self.cpu.reg_read(UC_ARM_REG_PC) == 0x03000000

    def called(self, name):
        return [args for n, args in self.calls if n == name]


def main():
    m = Interpreter()
    checks = 0
    # Every opcode's PC consumption, including all four argument bytes = zero.
    for code in range(16):
        data = [code, 0] if code in (1, 6, 10, 12, 13) else [code]
        if code == 5:
            data = [5, 0x20, 5]
        m.reset(data)
        m.step()
        assert m.u32(0x10C) == SCRIPT + len(data), hex(code)
        checks += 1
    for code in (6, 10, 12, 13):
        for arg in range(256):
            m.reset([code, arg])
            m.step()
            assert m.u32(0x10C) == SCRIPT + 2
            field = {6: 0x128, 10: 0x138, 12: 0x12E, 13: 0x138}[code]
            assert m.u8(field) == arg
            if code == 10:
                expected = {0: (0, 7, 1), 3: (20, 3, 3), 4: (1, 7, 4)}
                assert [a[:3] for a in m.called('menu')] == ([expected[arg]] if arg in expected else [])
                assert m.u8(0x128) == 20
            checks += 1
    m.reset([10, 0]); m.flag = 1; m.step()
    assert m.called('menu')[0][:3] == (0, 7, 2)
    m.reset([9]); m.step()
    assert m.called('menu')[0][:3] == (21, 7, 7)
    m.step(); assert not m.u8(0x126) & 4
    # 07 displays the indicator after the actor-busy gate, without requiring input.
    m.reset([7]); m.step()
    m.cpu.mem_write(0x02020013, b'\1')
    m.cpu.mem_write(0x03004278, b'\x20')
    m.step(); assert m.u8(0x126) & 4 and not m.called('prompt')
    m.cpu.mem_write(0x03004278, b'\0')
    m.step(); assert not m.u8(0x126) & 4 and m.called('prompt')[0][2] == 1
    # 08 accepts A or any D-pad edge, but not B/Start/Select/L/R; busy actors gate it.
    for key in (0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512):
        m.reset([8]); m.step(); m.keys(key); m.step()
        assert bool(m.u8(0x126) & 4) == (not bool(key & 0xF1))
        assert bool(m.called('sound')) == bool(key & 0xF1)
        checks += 1
    m.reset([8]); m.step(); m.keys(1)
    m.cpu.mem_write(0x02020013, b'\1'); m.cpu.mem_write(0x03004278, b'\x20')
    m.step(); assert m.u8(0x126) & 4
    # Delay decrements normally but fast budget bypasses it.
    m.reset([6, 64, 0x24]); m.step(); m.step(False)
    assert m.u8(0x128) == 63 and not m.called('glyph')
    m.write(0x124, 1, 2); m.step(False)
    assert m.called('glyph')[0][0] == 0x24
    # Newline moves two tile rows; overflow becomes the four-step scroll command.
    m.reset([2]); m.write(0x132, 40, 2); m.step()
    assert m.u8(0x132) == 0 and m.u8(0x134) == 2
    m.reset([2]); m.write(0x134, 2); m.step()
    assert m.u8(0x135) == 3 and m.u8(0x138) == 4
    for _ in range(4): m.step()
    assert len(m.called('scroll-step')) == 4 and not m.u8(0x126) & 4
    m.reset([3]); m.step()
    m.cpu.mem_write(DEST, b'\x22'*(26*64) + b'\x33'*(26*64))
    for _ in range(4): m.step()
    assert len(m.called('scroll-step')) == 4
    assert m.cpu.mem_read(DEST,26*64) == b'\x33'*(26*64)
    assert m.cpu.mem_read(DEST+26*64,26*64) == b'\x11'*(26*64)
    m.reset([5,0x20,0x21,5]); m.step()
    assert m.u8(0x13E) == 2 and m.u8(0x13F) == 1
    assert m.cpu.mem_read(DEST+0xD00,128) == m.cpu.mem_read(0x0873CAE8+0x20*64,128)
    # Plain/menu 02 aligns to an 8px column; each glyph has a leading pixel.
    for raw,tiles in [(bytes([0x10,0]),2),(bytes([0x10,2,0x10,0]),4),
                      (bytes([0x10,2,2,0x10,0]),4)]:
        m.cpu.mem_write(SCRIPT,raw)
        for reg,value in zip(REGS,(DEST,SCRIPT,0,0)):m.cpu.reg_write(reg,value)
        m.cpu.reg_write(UC_ARM_REG_SP,0x03007000)
        m.cpu.reg_write(UC_ARM_REG_LR,0x03000001)
        m.cpu.emu_start(0x08096C41,0x03000000,count=100000)
        assert m.cpu.reg_read(UC_ARM_REG_PC) == 0x03000000
        assert m.cpu.reg_read(UC_ARM_REG_R0) == tiles

    m.reset([4]); m.write(0x132, 50, 2); m.write(0x134, 2); m.step()
    assert m.u8(0x132) == m.u8(0x134) == 0
    assert m.cpu.mem_read(DEST, 26*4*32) == b'\x11' * (26*4*32)
    m.reset([11]); m.step()
    assert m.called('hide-window') and m.u8(0x128) == 20
    m.step(); assert m.called('show-window') and m.u8(0x12A) == 13
    # Dynamic text uses a BYTE index; 01 prefixes a glyph, other sub-10 bytes end it.
    for terminator in (0, 2, 5, 15):
        m.reset([13, 2]); m.cpu.mem_write(C + 0x114, bytes([0, 0, 0x24, 1, 0x40, terminator]))
        for _ in range(4): m.step()
        assert [a[0] for a in m.called('glyph')] == [0x24, 0x140]
        assert not m.u8(0x126) & 4
        checks += 1
    m.reset([14]); m.cpu.mem_write(0x02010280, struct.pack('<5H', 0x24, 0x140, 0x25, 0x26, 0))
    for _ in range(6): m.step()
    assert [a[0] for a in m.called('glyph')] == [0x24, 0x140, 0x25, 0x26]
    m.reset([15, 0x24]); m.step(); assert not m.called('glyph') and m.u8(0x126) == 1
    m.step(); assert m.called('glyph')[0][0] == 0x24
    m.reset([0]); m.step(); assert m.u8(0x126) == 0
    print(f'{checks} opcode/argument cases plus continuation, input, delay, clear and substitution checks passed.')


if __name__ == '__main__':
    main()
