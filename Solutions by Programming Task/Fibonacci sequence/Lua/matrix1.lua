Mat = {}

function Mat.new(dat)
    local mat = { data = {} }
    setmetatable(mat, {
        __mul = Mat.mul,
        __pow = Mat.pow,
    })
    mat.clone = Mat.clone

    if dat ~= nil then
        for i = 1, #dat do
            mat.data[i] = {}
            for j = 1, #dat[1] do
                mat.data[i][j] = dat[i][j]
            end
        end
    end
    return mat
end

function Mat:clone()
    return Mat.new(self.data)
end

function Mat.mul(lhs, rhs)
    local a, b = lhs.data, rhs.data
    assert(#a[1] == #b)

    local res_mat = Mat.new()
    local res = res_mat.data
    for i = 1, #a do
        res[i] = {}
        for j = 1, #b[1] do
            res[i][j] = 0
            for k = 1, #a do
                res[i][j] = res[i][j] + a[i][k] * b[k][j]
            end
        end
    end

    return res_mat
end

function Mat.pow(lhs, rhs)
    local x = lhs.data
    local len = #x
    assert(len == #x[1])
    assert(rhs >= 0, "not impl")

    local res_mat = lhs:clone()
    local res = res_mat.data
    if rhs == 0 then
        for i = 1, len do
            for j = 1, len do
                res[i][j] = i == j and 1 or 0
            end
        end
    elseif rhs < 0 then
        error("not impl")
    elseif rhs == 1 then
        -- skip
    else -- rhs > 1
        for _ = 2, rhs do
            res_mat = res_mat * lhs
        end
    end

    return res_mat
end

local function fib(x)
    return (Mat.new({ { 1, 1 }, { 1, 0 } }) ^ x).data[1][2]
end

for x = 0, 10 do
    print(fib(x))
end
