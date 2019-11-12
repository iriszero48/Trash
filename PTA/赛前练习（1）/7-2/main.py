from collections import *
if input() == '0':
    print()
else:
    r=[i.replace(' ','') for i in str(Counter(list(map(int,input().split())))).split('{')[1].split('}')[0].split(',')]
    r=sorted(r,key=lambda a:int(a.split(':')[0]))
    for i in r:
        print(i)
