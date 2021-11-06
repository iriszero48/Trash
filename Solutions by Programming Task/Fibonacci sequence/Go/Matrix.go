package main

import (
	"fmt"
)

func Fib(x int) int {
	powTow := 0
	if (x & -x) == x {
		powTow = x
	} else {
		powTow = x - 1
		powTow |= powTow >> 1
		powTow |= powTow >> 2
		powTow |= powTow >> 4
		powTow |= powTow >> 8
		powTow |= powTow >> 16
		powTow += 1
		powTow /= 2
	}
	q, r, i, s := 1, 1, 1, 0
	for i < powTow {
		i, q, r, s = i * 2, q * q + r * r, r * (q + s), r * r + s * s
	}
	for i < x {
		i, q, r, s = i + 1, q + r, q, r
	}
	return r
}

func main() {
	fmt.Println(Fib(10))
}
