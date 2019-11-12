input()
cin=list(map(int,input().split()))
print('average = ' + str(round(sum(cin)/len(cin),1)))
print('count = ' + str(round(len([i for i in cin if i >= 60]))))
