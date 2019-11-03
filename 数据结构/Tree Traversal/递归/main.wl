preorder[a_] := a;
preorder[a_[b__]] := Flatten@{a, preorder /@ {b}};
inorder[a_] := a;
inorder[a_[b_, c_]] := Flatten@{inorder@b, a, inorder@c};
inorder[a_[b_]] := Flatten@{inorder@b, a};
postorder[a_] := a;
postorder[a_[b__]] := Flatten@{postorder /@ {b}, a};
