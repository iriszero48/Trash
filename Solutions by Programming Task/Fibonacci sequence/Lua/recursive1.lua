local function fib(x)
    return x < 2 and x or fib(x - 1) + fib(x - 2)
end

for x = 0, 10 do
    print(fib(x))
end
