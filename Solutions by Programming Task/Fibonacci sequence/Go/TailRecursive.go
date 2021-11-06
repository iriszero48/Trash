package main

import "fmt"

func FibImpl(p int, n int, i int) int {
	if i == 0 {
		return n
	}
	return FibImpl(p + n, p, i - 1)
}

func Fib(x int) int {
	return FibImpl(1, 0, x)
}

func main() {
	fmt.Println(Fib(10))
}
