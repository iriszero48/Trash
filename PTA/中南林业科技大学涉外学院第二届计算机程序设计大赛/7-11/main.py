from collections import Counter
s=input()
r=sorted(list(map(lambda x: x.split(': ')[0],str(Counter(list(map(int,list(s)))))[9:-2].split(', '))),reverse=True)
print('int[] arr = new int[]{'+ ','.join(r) +'};')
print('int[] index = new int[]{'+ ','.join(list(map(str,[r.index(i) for i in s]))) +'};',end='')
