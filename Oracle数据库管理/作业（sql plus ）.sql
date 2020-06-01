-- 在SQL Plus执行一个SQL语句，然后进行语句编辑、保存脚本、执行脚本、并把显示结果保存下来
with fib(p,n,x) as (select 0,1,0 from dual union all select n,p+n,x+1 from fib where x<10) select max(p) from fib;
c/max(p)/p;
r
sav fib.sql;
spo ...txt;
@fib.sql;
spo off;
