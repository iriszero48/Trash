local function fib(x)
    return 2 ^ -x * ((1 + math.sqrt(5)) ^ x - (-1 + math.sqrt(5)) ^ x * math.cos(math.pi * x)) / math.sqrt(5)
end

for x = 0, 10 do
    print(fib(x))
end
