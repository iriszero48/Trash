local fib = setmetatable({ 1, 1 }, { __index = function(p, n)
    return n < 1 and n or p[n - 1] + p[n - 2]
end })

for x = 0, 10 do
    print(fib[x])
end
