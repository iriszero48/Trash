from collections import Counter
cin=str(Counter(list(map(int,input().split())))).split('{')[1]
print(cin.split(':')[0] + cin.split(':')[1].split(',')[0])
