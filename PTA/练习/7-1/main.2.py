print(reduce(lambda a,b:str(a)+'\n'+str(b),[i for i in range(1,int('1'+'0'*int(input()))) if i == sum(map(lambda y: y*y*y*y*y,map(int,list(str(i)))))]))
