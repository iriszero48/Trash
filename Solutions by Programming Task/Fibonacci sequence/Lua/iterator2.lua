local function fib_iter(x)
    local function iter(st)
        local i, p, n = table.unpack(st)
        local v = p
        st[1], st[2], st[3] = i + 1, n, p + n
        return i < x and v or nil
    end

    local st = { -1, 0, 1 }
    return iter, st
end

for v in fib_iter(10) do
    print(v)
end
