package main

import "fmt"

func Fib(x int) int {
	impl := func(x chan int) {
		p, n := 0, 1
		for {
			x <- p
			p, n = n, p + n
		}
	}
	res := make(chan int)
	go impl(res)
	for range make([]int, x) {
		<-res
	}
	return <-res
}

func main() {
	fmt.Println(Fib(10))
}
