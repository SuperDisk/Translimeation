"""Verify asset extraction and a relocation using the original ROM routines."""
import _paths

import struct
from pathlib import Path
from slime_gfx import Decompressor
from unicorn import Uc, UC_ARCH_ARM, UC_MODE_THUMB

def create_cpu(rom):
 cpu=Uc(UC_ARCH_ARM,UC_MODE_THUMB)
 # ARMv4T POP {pc} retains Thumb state even for an even return address.
 cpu.ctl_set_cpu_model(UC_CPU_ARM_TI925T)
 for a,n in ((0x08000000,(len(rom)+4095)&~4095),(0x02000000,0x40000),(0x03000000,0x8000)):
  cpu.mem_map(a,n)
 cpu.mem_write(0x08000000,rom)
 return cpu
from unicorn.arm_const import *
from audit_graphics import ARCHIVES,archive_entries
rom=Path('slime_original.gba').read_bytes();cpu=create_cpu(rom)
regs=(UC_ARM_REG_R0,UC_ARM_REG_R1,UC_ARM_REG_R2,UC_ARM_REG_R3)
def call(cpu,address,*args):
 for reg,v in zip(regs,args):cpu.reg_write(reg,v)
 cpu.reg_write(UC_ARM_REG_SP,0x03007000);cpu.reg_write(UC_ARM_REG_LR,0x03000001)
 cpu.emu_start(address|1,0x03000000,count=10000000)
 assert cpu.reg_read(UC_ARM_REG_PC)==0x03000000
 return cpu.reg_read(UC_ARM_REG_R0)
entries={}
for archive in ARCHIVES:
 for i,p,n in archive_entries(rom,archive):
  assert call(cpu,0x08000858,0x08000000+archive,i)==0x08000000+p
  assert call(cpu,0x0800086c,0x08000000+archive,i)==n
  entries[archive,i]=(p,n)
print(len(entries),'archive addresses and sizes match the actual ROM lookup routines')
for p in [entries[0x765fa8,i][0] for i in (0x19f,0x1a1,0x1a2,0x1a3,0x2bd)]+[0x558bcc,0x5597d4]:
 expected,_,_=Decompressor(rom,p).decompress();cpu.mem_write(0x02020000,b'\xa5'*(len(expected)+32))
 returned=call(cpu,0x08098ac8,0x08000000+p,0x02020000)
 assert returned==len(expected)
 assert bytes(cpu.mem_read(0x02020000,len(expected)))==expected,hex(p)
print('7 title/tutorial streams match the ROM decompressor')
# Prove the relocation mechanism with an unchanged title tileset, encoded in
# the engine's supported literal-only mode 04; no compressor implementation needed.
p,n=entries[0x765fa8,0x19f];data=Decompressor(rom,p).decompress()[0]
bits=b'\x04'+data;bits+=bytes(-len(bits)%4)
packed=struct.pack('<I',(len(data)<<8)|0x70)+b''.join(bits[i:i+4][::-1] for i in range(0,len(bits),4))
assert Decompressor(packed,0).decompress()[0]==data
copy=bytearray(rom);new=len(copy);copy.extend(packed)
count=struct.unpack_from('<I',copy,0x765fa8)[0];base=0x765fa8+4+count*8
struct.pack_into('<II',copy,0x765fa8+4+0x19f*8,new-base,len(packed))
cpu=create_cpu(bytes(copy));ptr=call(cpu,0x08000858,0x08765fa8,0x19f)
assert ptr==0x08000000+new
call(cpu,0x08098ac8,ptr,0x02020000)
assert bytes(cpu.mem_read(0x02020000,len(data)))==data
print('Relocated title entry decoded identically using original game code; mode 04 works')

# The pot title is a timed OBJ cell sequence over a separately changing BG.
# Check the exported timeline against the original selector/update routines.
from graphics_pot import animation, timeline
p, n = entries[0x1d9fec, 0xbe]
cells = rom[p:p+n]
_, commands = animation(cells)
cpu = create_cpu(rom)
context = 0x02020000
# 080020C4 clears the context via DMA and sets its first two pointers. Unicorn
# does not model GBA DMA, so supply that initial state before testing animation.
cpu.mem_write(context, struct.pack('<II', 0x08000000+p+4,
                                  0x08000000+p+(struct.unpack_from('<H', cells)[0]&~1)) + bytes(16))
call(cpu, 0x08002140, context, 0)
for expected_cell in timeline(commands, 190):
 call(cpu, 0x0800188c, context)
 assert cpu.mem_read(context+0xe, 1)[0] == expected_cell
print('190 pot-title animation ticks match the original ROM interpreter')
