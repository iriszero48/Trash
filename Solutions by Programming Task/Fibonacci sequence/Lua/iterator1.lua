local function fib_iter(x)
    local i, p, n = -1, 0, 1
    return function()
        local v = p
        i, p, n = i + 1, n, p + n
        return i <= x and v or nil
    end
end

for v in fib_iter(10) do
    print(v)
end
