-module(kata).
-export([strong/1]).

strong(N) ->
  case lists:sum(lists:map(fun(X) -> lists:foldl(fun(X,Y) -> X*Y end, 1, lists:seq(1,X-48)) end, integer_to_list(N))) of
    N -> "STRONG!!!!";
    _ -> "Not Strong !!"
  end.
