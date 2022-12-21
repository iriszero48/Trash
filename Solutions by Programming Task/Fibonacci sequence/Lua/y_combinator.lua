local function fib(a)
    return (function(f) return (function(x)
            return x(x)
        end)(function(y)
            return f(function(...)
                return y(y)(...)
            end)
        end)
    end)(function(f)
        return function(n)
            return n < 2 and n or f(n - 1) + f(n - 2)
        end
    end)(a)
end

for x = 0, 10 do
    print(fib(x))
end
