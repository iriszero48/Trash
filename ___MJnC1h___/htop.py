from socket import *
s = socket()
s.connect((gethostname(),5555))
s.send('ls /')
print(s.recv(4096))
