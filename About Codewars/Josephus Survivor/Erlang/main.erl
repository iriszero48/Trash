-module(kata).
-export([josephus_survivor/2]).

js([S], _, _) -> S;
js(L, I, K) ->
    NI = (I + K) rem length(L) + 1,
    js(lists:delete(lists:nth(NI, L), L), NI - 1, K).

josephus_survivor(N, K) -> js(lists:seq(1, N), 0, K - 1).
