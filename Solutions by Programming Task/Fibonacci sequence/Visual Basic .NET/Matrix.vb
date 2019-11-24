Imports System.Windows.Media
Function Fib(x As Integer) As Double
    If x < 2 Then Return x
    Dim matrix = New Matrix(1, 1, 1, 0, 0, 0)
    Dim res = New Matrix(1, 1, 1, 0, 0, 0)
    For i = 0 To x - 3
        res = Matrix.Multiply(res, matrix)
    Next
    Return res.M11
End Function
