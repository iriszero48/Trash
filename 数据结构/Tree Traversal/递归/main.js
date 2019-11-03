var v = 0, l = 1, r = 2,
Preorder = n => [n[v]].concat(n[l] ? Preorder(n[l]) : []).concat(n[r] ? Preorder(n[r]) : []),
Inorder = n => (n[l] ? Inorder(n[l]) : []).concat(n[v]).concat(n[r] ? Inorder(n[r]) : []),
Postorder = n => (n[l] ? Postorder(n[l]) : []).concat(n[r] ? Postorder(n[r]) : []).concat(n[v]),
t = [1,
    [2,
        [4,
            [7]
        ],
        [5]],
    [3,
        [6,
            [8],
            [9]
        ]
    ]];
[Preorder, Inorder, Postorder].map(f => console.log(f(t)));
