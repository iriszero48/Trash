local function fib(x)
    local function f(a, b, p, q, i)
        if i == 0 then
            return b
        end
        return i % 2 == 0
            and f(a, b, p ^ 2 + q ^ 2, q ^ 2 + 2 * p * q, i // 2)
            or f(b * q + a * q + a * p, b * p + a * q, p, q, i - 1)
    end

    return f(1, 0, 0, 1, x)
end

for x = 0, 10 do
    print(fib(x))
end
