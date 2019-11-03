-module(traversal).
-export([main/0]).

tree() -> {}.
tree(V) -> {V, {}, {}}.
tree(V, L, R) -> {V, L, R}.

preorder(_, {}) -> ok;
preorder(F, {V, L, R}) -> F(V), preorder(F, L), preorder(F, R).

inorder(_, {}) -> ok;
inorder(F, {V, L, R}) -> inorder(F, L), F(V), inorder(F, R).

postorder(_, {}) -> ok;
postorder(F, {V, L, R}) -> postorder(F, L), postorder(F, R), F(V).

main() ->
    T = 
        tree(1,
            tree(2,
                tree(4,
                    tree(7),
                    tree()),
                tree(5)),
            tree(3,
                tree(6,
                    tree(8),
                    tree(9)),
                tree())),
    F = fun(X) -> io:format("~p ", [X]) end,
    preorder(F, T), io:format("~n"), inorder(F, T), io:format("~n"), postorder(F, T).
