if input() == '0':
    print('0 0')
else:
    cin=list(map(int,input().split()))
    print(str(len([i for i in cin if i%2!=0]))+' '+str(len([i for i in cin if i%2==0])))
