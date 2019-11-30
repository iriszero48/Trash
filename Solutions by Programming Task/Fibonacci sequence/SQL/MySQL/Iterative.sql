create function Fib(x int) returns int deterministic
begin
    declare p int default 0;
    declare n int default 1;
    declare sum int;
    declare i int default 0;
    while i < x do
        set sum = p + n;
        set p = n;
        set n = sum;
        set i = i + 1;
    end while;
    return p;
end;
