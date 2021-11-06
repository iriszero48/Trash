package main

import "fmt"

func Fib(x int) int {
	p, n := 0, 1
	for range make([]int, x) {
		p, n = n, p + n
	}
	return p
}

func main() {
	fmt.Println(Fib(10))
}
