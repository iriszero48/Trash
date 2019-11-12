list(map(print,[i for i in range(1,int('1'+'0'*int(input()))) if i == sum(list(map(lambda y: y*y*y*y*y,list(map(int,list(str(i)))))))]))
