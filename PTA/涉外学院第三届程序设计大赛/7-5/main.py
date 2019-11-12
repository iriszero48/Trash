def f(x,y):
    for i in range(len(a)):
        if x[i:] == y[:len(x[i:])]:
            return len(x[i:])
    return 0

a=input()
b=input()
c=input()
print(f(a,b),end=' ')
print(f(b,c),end='')
