import socket
ef=444444444444444444444444444
sf=333333333333333333333333333
f=(ef+sf)//2

while True:
	s = socket.socket()
	s.connect(('183.129.189.60',10029))
	s.send(str(f).encode())
	rev = s.recv(1024).decode()
	print(f, rev)
	if rev == 'too big':
		ef = f
	elif rev == 'too small':
		sf=f
	else:
		break;
	s.close()
	f=(ef+sf)//2
print(f)
