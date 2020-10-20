#1、编写程序，输入任意大的自然数，输出各位数字之和。
print(sum(map(int, input('请输入一个自然数:'))))
#2、编写程序，输入两个集合 setA 和 setB，分别输出它们的交集、并集和差集 setA-setB。
a = set(input('请输入第1个集合:').split())
b = set(input('请输入第2个集合:').split())
print('交集', a & b)
print('并集', a | b)
print('差集', a - b)
#3、编写程序，输入一个自然数，输出它的二进制、八进制、十六进制表示形式。
x = int(input('请输入一个自然数:'))
print('二进制', bin(x))
print('八进制', oct(x))
print('十六进制', hex(x))
