package main

import "fmt"

func Fib(x int) int {
	if x < 2 {
		return x
	}
	return Fib(x - 1) + Fib(x - 2)
}

func main() {
	fmt.Println(Fib(10))
}
