var 
    n,i:longint;
    b:array[1..1000000] of longint;
function Pow10(x:longint):longint;
begin
    Pow10:=1;
    for i:=1 to x do
        Pow10:=Pow10*10;
end;
function Judge(x:longint):longint;
var 
    s:string;
    i,si,code:longint;
begin
    Judge:=0;
    Str(x,s);
    for si:=1 to length(s) do
    begin
        Val(s[si],i,code);
        Judge:=Judge+i*i*i*i*i;
    end;
end;
begin
    readln(n);
    For i:=1 to Pow10(n) do
        b[i]:=i;
    For i:=1 to Pow10(n) do
        if i = Judge(b[i]) then
            writeln(i);
end.
