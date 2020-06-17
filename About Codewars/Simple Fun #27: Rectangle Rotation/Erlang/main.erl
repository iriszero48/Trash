-module(kata).
-export([rectangle_rotation/2]).

rectangle_rotation(A, B) ->
  I = [math:floor(X / math:sqrt(2)) || X <- [A, B]],
  R = round(lists:sum([lists:foldl(fun(X, S) -> X * S end, 1, X) || X <- [I, [X + 1 || X <- I]]])),
  if (R band 1) == 0 -> R - 1; true -> R end.
