fib[x_] := Block[{p = 0, n = 1, sum},
  For[i = 0, i < x - 1 , i++,
    sum = p + n;
    p = n;
    n = sum];
  Return[n]]
