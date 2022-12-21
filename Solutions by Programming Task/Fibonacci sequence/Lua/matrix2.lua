local function fib(x)
    if x == 0 then
        return 0
    end

    local function prev_pow_two(v)
        if (v & -v) == v then
            return v
        else
            v = v - 1
            v = v | (v >> 1)
            v = v | (v >> 2)
            v = v | (v >> 4)
            v = v | (v >> 8)
            v = v | (v >> 16)
            v = v + 1
            return v / 2
        end
    end

    local pow_two = prev_pow_two(x)
    local q, r, i, s = 1, 1, 1, 0
    while i < pow_two do
        i, q, r, s = i * 2, q ^ 2 + r ^ 2, r * (q + s), r ^ 2 + s ^ 2
    end
    while i < x do
        i, q, r, s = i + 1, q + r, q, r
    end
    return r
end

for x = 0, 10 do
    print(fib(x))
end
