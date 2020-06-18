from collections import Counter
[print(str(k) + ':' + str(v)) for k,v in sorted(Counter(input()).items(), key=lambda pair: pair[0])]
