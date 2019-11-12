import itertools
i=int(input())
[print(''.join(list(map(str,list(o))))) for o in list(itertools.permutations(range(1,i+1),i))]
