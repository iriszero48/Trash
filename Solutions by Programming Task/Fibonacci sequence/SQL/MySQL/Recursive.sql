with recursive Fib(p, n, x) as (select 0, 1, 0 union all select n, p + n, x + 1 from Fib where x < 10) select max(p) from Fib;
