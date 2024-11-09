a := []
b := []
c := []
sumVal := 0.0

Loop 128 * 128
{
    ; a.Push(A_Index / (128. * 128.))
    a.Push(A_Index == 128 * 128 ? 1.0 : Random())
}

Loop 128 * 128
{
    b.Push(1.0)
}

Loop 128 * 128
{
    c.Push(0.0)
}

Add(&m, x)
{
    Loop 128 * 128
    {
        m[A_Index] += x
    }
}

Div(&m, x)
{
    Loop 128 * 128
    {
        m[A_Index] /= x
    }
}

Mul(&a, &b, &c)
{
    row := 0
    while row < 128
    {
        col := 0
        while col < 128
        {
            v := 0.0, i := 0
            while i < 128
            {
                v += a[row * 128 + i + 1] * b[i * 128 + col + 1]

                i++
            }
            c[row * 128 + col + 1] := v

            col++
        }

        row++
    }
}

DllCall("QueryPerformanceFrequency", "Int64*", &freq := 0)
DllCall("QueryPerformanceCounter", "Int64*", &tp1 := 0)

loop 1
{
    Add &b, A_Index - 1.0
    Mul &a, &b, &c

    Add &b, A_Index
    Mul &c, &b, &a

    Div &a, c[128 * 128]
}

for x in a
    sumVal += x

DllCall("QueryPerformanceCounter", "Int64*", &tp2 := 0)

; stdout := FileOpen("*", "w")
stdout := FileOpen("output.txt", "w")
stdout.WriteLine (tp2 - tp1) / freq * 1000 " " . sumVal
