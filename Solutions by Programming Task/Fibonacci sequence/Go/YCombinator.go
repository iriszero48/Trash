package main

import "fmt"

type FuncType func(int) int
type YFuncType func(FuncType) FuncType
type RecFuncType func (RecFuncType) FuncType

func Y(f YFuncType) FuncType {
	g := func(r RecFuncType) FuncType {
		return f(func(x int) int {
			return r(r)(x)
		})
	}
	return g(g)
}

var fib = Y(func(f FuncType) FuncType {
	return func(x int) int {
		if x <= 2 {
			return 1
		}
		return f(x-1)+f(x-2)
	}
})

func main() {
	fmt.Println(fib(10))
}
