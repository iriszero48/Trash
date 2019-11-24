Function Fib(x As Decimal) As Decimal
    Return 2 ^ -x * ((1 + Math.Sqrt(5)) ^ x - (-1 + Math.Sqrt(5)) ^ x * Math.Cos(Math.PI * x)) / Math.Sqrt(5)
End Function
