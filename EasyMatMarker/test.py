from subprocess import PIPE, Popen
import argparse
import statistics
import json

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--exec', required=True)
    parser.add_argument('-c', '--cycle', default=100, type=int)
    
    args = parser.parse_args()

    times = []
    for i in range(args.cycle):
        p = Popen(args.exec.split(' '), stdout=PIPE)
        times.append(float(p.stdout.read().decode().strip().split(' ')[0].strip()))
        print(f'{i + 1} -> {json.dumps(args.exec)},{statistics.mean(times)},{max(times)},{min(times)},{statistics.stdev(times) if len(times) > 1 else 0.}')
        
    with open('test.res.csv', 'a') as f:
        f.write(f',{json.dumps(args.exec)},{statistics.mean(times)},{max(times)},{min(times)},{statistics.stdev(times) if len(times) > 1 else 0.}\n')
