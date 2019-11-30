def fib(x)
  p, n = 0, 1
  x.times {p, n = n, p + n}
  p
end
