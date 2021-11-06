package main

import "fmt"

var dict = map[int] int {0:0, 1:1}

func Fib(x int) int {
	if res, ex := dict[x]; ex {
		return res
	}
	dict[x] = Fib(x - 1) + Fib(x - 2)
	return dict[x]
}

func main() {
	fmt.Println(Fib(10))
}
