package main

import (
	"fmt"
	"time"
	"math/rand/v2"
)

const ROW = 128
const COL = 128
const SIZE = ROW * COL

func add(mat []float64, val float64) {
	for i := range mat {
		mat[i] += val
	}
}

func div(mat []float64, val float64) {
	for i := range mat {
		mat[i] /= val
	}
}

func mul(a []float64, b []float64, c []float64) {
	for row := 0; row < ROW; row++ {
		for col := 0; col < COL; col++ {
			v := 0.
			for i := 0; i < COL; i++ {
				v += a[row*COL+i] * b[i*COL+col]
			}
			c[row*COL+col] = v
		}
	}
}

func main() {
	a := make([]float64, SIZE)
	b := make([]float64, SIZE)
	c := make([]float64, SIZE)
	sumValue := 0.

	for i := range a {
		// a[i] = float64(i+1.) / SIZE
		a[i] = rand.Float64()
	}
	a[128 * 128 - 1] = 1.0

	for i := range b {
		b[i] = 1.
	}

	tp1 := time.Now()

	for i := 0; i < 1000; i++ {
		add(b, float64(i))
		mul(a, b, c)

		add(b, float64(i)+1.)
		mul(c, b, a)

		div(a, c[127*COL+127])
	}

	for i := range a {
		sumValue += a[i]
	}

	tp2 := time.Now()

	fmt.Println(float64(tp2.Sub(tp1).Nanoseconds())/1000./1000., sumValue)
}
