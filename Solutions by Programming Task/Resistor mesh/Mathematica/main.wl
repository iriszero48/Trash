n = 10;
d1 = SparseArray[{{i_, i_} -> 1, {i_, j_} /; j - i == 1 -> -1}, {n - 1, n}];
eye = SparseArray[{{i_, i_} -> 1}, n];
d = ArrayFlatten[{{KroneckerProduct[d1, eye]}, {KroneckerProduct[eye, d1]}}];
x = n*1 + 2; y = n*7 + 7;
v = LinearSolve[Transpose[d].d, SparseArray[{x -> 1, y -> -1}, n*n]];
N[v[[x]] - v[[y]], 40]
