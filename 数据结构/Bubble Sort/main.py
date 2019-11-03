def BubbleSort(lst):
    def sort(accum,rev,lst):
        if lst == [] and rev:
           return list(reversed(accum))
        elif lst == [] and rev == False:
            return sort([], True, list(reversed(accum)))
        else:
            if len(lst) == 1:
                return sort(lst + accum, rev, [])
            x,y,tail = lst[:1],lst[1:2],lst[2:]
            if x > y:
                return sort(y + accum, False, x + tail)
            else:
                return sort(x + accum, rev, y + tail)
    return sort([],True,lst)

print(BubbleSort(list(map(int,input().split()))))
