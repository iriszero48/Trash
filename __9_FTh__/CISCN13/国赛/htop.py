from socket import *
s = socket()
s.connect((gethostname(),5555))
s.send('ls /'+'\r\n')
print(s.recv(4096))
