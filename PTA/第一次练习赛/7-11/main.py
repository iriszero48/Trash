m=int(input().split()[1])
a=list(map(int,input().split()))
r=a[m:]+a[:m]
print(' '.join(list(map(str,r))),end='')
