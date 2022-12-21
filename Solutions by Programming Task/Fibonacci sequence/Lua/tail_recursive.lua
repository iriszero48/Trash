local function fib(x)
    local function f(p, n, i)
        return i == 0 and n or f(p + n, p, i - 1)
    end

    return f(1, 0, x)
end

for x = 0, 10 do
    print(fib(x))
end
