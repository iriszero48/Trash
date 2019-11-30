create function Fib(x int) returns int deterministic return POWER((1 + SQRT(5)) / 2, x) / SQRT(5);
