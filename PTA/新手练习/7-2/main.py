cin=int(input())
if cin < 2001 or cin > 3000:
    print('Invalid year!')
else:
    r= [i for i in range(2001,cin+1) if (i%4==0 and i%100!=0) or (i%400==0)]
    if len(r) == 0:
        print('None')
    else:
        for i in r:
            print(i)
