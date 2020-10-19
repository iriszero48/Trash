#1．编写一个程序，其功能为：求1-100之间的奇数之和。
sum(range(1, 101)[::2])
#2、编写一个程序，其功能为：计算并输出一年12个月的总降雨量和平均降雨量。
(lambda x: [print(s, f(x)) for s, f in [('总降雨量', sum), ('平均降雨量', __import__('statistics').mean)]])([float(input('{}月降水量:'.format(i))) for i in range(1, 13)])
#3、编写一个程序，求1+2!+3!+…+20!的和。
sum(map(__import__('math').factorial, range(1, 21)))
