import std/times
import std/strformat
import std/random

const
    ROW = 128
    COL = 128
    SIZE = ROW * COL


type Matrix = array[SIZE, float64]


var
    a: Matrix
    b: Matrix
    c: Matrix
    sum_value = 0.0


proc mat_div(mat: var Matrix, x: float64) =
    for i in 0..<SIZE:
        mat[i] /= x


proc mat_add(mat: var Matrix, x: float64) =
    for i in 0..<SIZE:
        mat[i] += x


proc mat_mul(a: Matrix, b: Matrix, c: var Matrix) =
    for row in 0..<ROW:
        for col in 0..<COL:
            var v = 0.0
            for i in 0..<COL:
                v += a[row * 128 + i] * b[i * 128 + col]
            c[row * 128 + col] = v

randomize()
for i in 0..<SIZE:
    # a[i] = (i + 1) / SIZE
    a[i] = rand(1.0)
a[127 * 128 + 127] = 1.0
    
for i in 0..<SIZE:
    b[i] = 1

for i in 0..<SIZE:
    c[i] = 0

let tp1 = cpuTime()

for i in 0..<1000:
    mat_add(b, i.float)
    mat_mul(a, b, c)

    mat_add(b, i.float + 1.0)
    mat_mul(c, b, a)

    mat_div(a, c[127 * 128 + 127])

for i in 0..<SIZE:
    sum_value += a[i]

let tp2 = cpuTime()

echo &"{(tp2 - tp1) * 1000.0} {sum_value}"
