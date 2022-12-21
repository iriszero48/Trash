Dict = {}
Dict[0] = 0
Dict[1] = 1
Dict[2] = 1

local function fib(x)
    local val = Dict[x]
    if val ~= nil then
        return val
    end

    local f1 = fib(x // 2 + 1)
    local f2 = fib((x - 1) // 2)
    Dict[x] = x & 1 == 1 and f1 ^ 2 + f2 ^ 2 or f1 ^ 2 - f2 ^ 2
    return Dict[x]
end

for x = 0, 10 do
    print(fib(x))
end
