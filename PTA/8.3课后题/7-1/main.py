def search(l,x):
    try:
        return 'index = '+str((sorted(l).index(x)-1))
    except:
        return 'Not found'
res=[]
for i in range(int(input())):
    input()
    res.append(search(list(map(int,input().split())),int(input())))
for r in res:
    print(r)
