from functools import *
list(map(lambda _i:print(reduce(lambda a,b:a+' '+b,(list(map(lambda x: x[::-1],input().split(' ')))))),range(int(input()))))
