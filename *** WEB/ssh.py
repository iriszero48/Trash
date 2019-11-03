import paramiko,os,sys
class SSH(object):
    def __init__(self, ip, port, user, pw):
        self.ssh = paramiko.SSHClient()
        self.ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        self.ssh.connect(ip, port, user, pw)

    def Send(self, path):
        with open(path,'r') as f:
            r=f.read().split('\n')
        for i in r:
            stdin, stdout, stderr = self.ssh.exec_command(i)
            print stdout.read().decode("utf-8")
            print stderr.read().decode("utf-8")

s=SSH(sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5])
s.Send(sys.argv[1])
