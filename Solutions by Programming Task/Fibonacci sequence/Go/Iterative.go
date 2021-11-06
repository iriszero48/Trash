package main

import "fmt"

func Fib(x int) int {
	d := [2]int{0,1}
	for range make([]int, x) {
		sum := d[0] + d[1]
		d[0] = d[1]
		d[1] = sum
	}
	return d[0]
}

func main() {
	fmt.Println(Fib(10))
}
