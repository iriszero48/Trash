local fib = coroutine.create(function()
    local p, n = 0, 1
    while true do
        coroutine.yield(p)
        p, n = n, p + n
    end
end)

for _ = 0, 10 do
    local _, v = coroutine.resume(fib)
    print(v)
end
