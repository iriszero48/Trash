def p(x):
    for i in range(2,int(pow(x,0.5))):
        if x % i == 0:
            return False
    return True

for t in range(int(input())):
    cin=int(input())
    if p(cin):
        print('Yes')
    else:
        print('No')
