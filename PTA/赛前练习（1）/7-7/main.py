def cout(x):
    ng=False
    x=str(x)
    if len(x.split('+')) == 1:
        if x[0] == '-':
            x[0]='+'
            x=str(x).replace('(','').replace(')','').split('*')
            x[0][0]='-'
            ng=True
        else:
            x=str(x).replace('(','').replace(')','').split('-')
            ng=True
    else:
        x=str(x).replace('(','').replace(')','').split('+')
    if len(x) == 1:
        x=x[0].split('j')
        if len(x) == 2:
            return str(round(float(x[0]),1))+'i'
        else:
            return str(round(float(x[0]),1))
    else:
        x=list(map(lambda i:str(round(float(i.replace('j','')),1)),x))
        if(x[0]=='0.0' or x[0] == '-0.0'):
            return x[1]+'i'
        else:
            if ng ==True:
                return x[0]+'-'+x[1]+'i'
            else:
                return x[0]+'+'+x[1]+'i'

cin=list(map(lambda x:float(x),input().split()))
a=complex(cin[0],cin[1])
b=complex(cin[2],cin[3])
print('('+cout(a)+') + ('+cout(b)+') = '+cout(a+b))
print('('+cout(a)+') - ('+cout(b)+') = '+cout(a-b))
print('('+cout(a)+') * ('+cout(b)+') = '+cout(a*b))
print('('+cout(a)+') / ('+cout(b)+') = '+cout(a/b))
