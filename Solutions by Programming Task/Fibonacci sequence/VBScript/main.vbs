Function Fib(x)
    If x < 2 Then
        Fib = x
    else 
        Fib = Fib(x - 1) + Fib(x - 2)
    End If
End Function

msgbox(fib(inputbox("x:","x")))
