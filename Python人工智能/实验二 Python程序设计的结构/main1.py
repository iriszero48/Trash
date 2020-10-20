#1．编写一个程序，其功能为：求1-100之间的奇数之和。
total = 0
for i in range(1, 101):
    if i % 2 == 1:
        total += i
print(total)
#2、编写一个程序，其功能为：计算并输出一年12个月的总降雨量和平均降雨量。
total = 0
for i in range(1, 13):
    total += float(input('请输入{}月份的降水量:'.format(i)))
print('总降雨量', total)
print('平均降雨量', total / 12)
#3、编写一个程序，求1+2!+3!+…+20!的和。
total = 0
for i in range(1, 21):
    fac = 1
    for x in range(1, i + 1):
        fac *= x
    total += fac
print(total)
