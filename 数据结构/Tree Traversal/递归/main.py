node = lambda v,l,r:[v,l,r]
leaf = lambda v:[v,[],[]]
null = []

def Preorder(f,t):
    if not t == []:
        v,l,r = t
        f(v)
        Preorder(f,l)
        Preorder(f,r)

def Inorder(f,t):
    if not t == []:
        v,l,r = t
        Inorder(f,l)
        f(v)
        Inorder(f,r)

def Postorder(f,t):
    if not t == []:
        v,l,r = t
        Postorder(f,l)
        Postorder(f,r)
        f(v)

t = node(1,
        node(2,
            node(4,
                leaf(7),
                null),
            leaf(5)),
        node(3,
            node(6,
                leaf(8),
                leaf(9)),
            null))
f = lambda x:print(x,end=' ')

Preorder(f,t)
print()
Inorder(f,t)
print()
Postorder(f,t)
