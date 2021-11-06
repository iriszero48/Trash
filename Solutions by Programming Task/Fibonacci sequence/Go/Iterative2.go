package main

import "fmt"

func Fib(x int) int {
	impl := func() func() int {
		p, n := 0, 1
		return func() int {
			p, n = n, p + n
			return p
		}
	}()
	res := 0
	for range make([]int, x) {
		res = impl()
	}
	return res
}

func main() {
	fmt.Println(Fib(10))
}
