sin=sorted(list(input()))
#print(sin)
for i in range(1,1000000000):
    a=int(''.join(sin[::-1]))
    b=int(''.join(sin))
    r=a-b;
    print(str(i)+': '+str(a)+' - '+str(b)+' = '+str(r))
    sin=sorted(list(str(r)))
    if(r == 495):
        break
