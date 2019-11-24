Imports System.Numerics
Function Fib(x As BigInteger) As BigInteger
    If x < 2 Then Return x
    Dim p As BigInteger = 0
    Dim n As BigInteger = 1
    For i = 0 To x - 2
        Dim s = p + n
        p = n
        n = s
    Next
    Return n
End Function
