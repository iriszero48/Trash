-module(main).
-export([main/0]).

main() ->
    {ok, Listen} = gen_tcp:listen(8852,[binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    do(Listen, gen_tcp:accept(Listen)),
    gen_tcp:close(Listen).

do(Listen, {ok, Socket}) -> 
    receive
        {tcp, Socket, _} ->
            gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>">>),
            gen_tcp:close(Socket)
    end,
    do(Listen, gen_tcp:accept(Listen)).
