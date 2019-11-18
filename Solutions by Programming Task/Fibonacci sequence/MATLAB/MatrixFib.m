function [res] = MatrixFib(x)
    r = [1 1; 1 0] ^ (x - 1);
    res = r(1, 1);
end
