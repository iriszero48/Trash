package main

import (
	"fmt"
	"math"
)

var Fib = func(x int) int {
	return int(math.Round(
		math.Pow(2, float64(-x)) *
		(math.Pow(1 + math.Sqrt(5), float64(x)) -
			math.Pow(-1 + math.Sqrt(5), float64(x)) *
			math.Cos(math.Pi * float64(x))) /
		math.Sqrt(5)))
}

func main() {
	fmt.Println(Fib(10))
}
