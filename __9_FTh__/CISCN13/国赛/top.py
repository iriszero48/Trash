from socket import *
s = socket()
s.bind((gethostname(),5555))
s.listen(100)
while True:
    c,addr = s.accept()
    print(addr)
    cmd='dir'
    c.send(cmd)
    print(c.recv(4096))
    c.close()
