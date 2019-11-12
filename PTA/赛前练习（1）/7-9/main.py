from collections import *
from functools import *
cin=input()
arr=sorted(Counter(list(map(int,[i for i in cin]))),reverse=True)
index = [reduce(lambda a,b:str(a)+str(b),arr).find(i) for i in cin]
print('int[] arr = new int[]{'+reduce(lambda a,b:str(a)+','+str(b),arr)+'};')
print('int[] index = new int[]{'+reduce(lambda a,b:str(a)+','+str(b),index)+'};')
