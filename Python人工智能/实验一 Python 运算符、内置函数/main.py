#1、编写程序，输入任意大的自然数，输出各位数字之和。
print(sum(map(int, input('请输入一个自然数:'))))
#2、编写程序，输入两个集合 setA 和 setB，分别输出它们的交集、并集和差集 setA-setB。
(lambda op, x: [print(s, __import__('functools').reduce(f, x)) for s, f in [('交集', op.and_), ('并集', op.or_), ('差集', op.sub)]])(__import__('operator'), [set(input('请输入第{}个集合:'.format(i)).split()) for i in range(2)])
#3、编写程序，输入一个自然数，输出它的二进制、八进制、十六进制表示形式。
[print(s, f(x)) for (s, f), x in zip([('二进制', bin), ('八进制', oct), ('十六进制', hex)], [int(input('请输入一个自然数:'))] * 3)]
