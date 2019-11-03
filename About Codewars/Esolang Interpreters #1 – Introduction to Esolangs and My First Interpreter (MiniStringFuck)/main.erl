-module(mini_string_fuck).
-export([my_first_interpreter/1]).

my_first_interpreter(Code) ->
    interpreter(Code,0,[]).

interpreter(Code, Value, Output) ->
    case Code of
        [] -> Output;
        [Head | Tail] when Head == 43 -> interpreter(Tail, 
            case Value > 254 of
                true -> 0;
                _ -> Value + 1
            end ,Output);
        [Head | Tail] when Head == 46 -> interpreter(Tail,Value,lists:append(Output,[Value]));
        [_ | Tail] -> interpreter(Tail,Value,Output)
    end.
