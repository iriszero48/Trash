Function Fib(x As Integer) As Integer
    If x < 2 Then Return x
    Return Fib(x - 1) + Fib(x - 2)
End Function
