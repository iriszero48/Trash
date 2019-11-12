def prime(x):
    for i in range(2,x):
        if x % i == 0:
            return False
    return True

def fuck(x):
    for i in range(3,x):
        if prime(i) and prime(x-i):
            return str(x) + ' = ' + str(i)+' + '+str(x-i)

print(fuck(int(input())),end='')
