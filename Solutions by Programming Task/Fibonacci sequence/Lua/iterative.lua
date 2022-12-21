local function fib(x)
    local p, n = 0, 1
    for _ = 0, x - 1 do
        p, n = n, p + n
    end
    return p
end

for x = 0, 10 do
    print(fib(x))
end
