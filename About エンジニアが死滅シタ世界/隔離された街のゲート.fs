open System
[<EntryPoint>]
let main _ = 
    let Y::X::n::_ = 
        Console.ReadLine().Trim().Split(' ') 
        |> List.ofArray 
        |> List.map Convert.ToInt32
    let rec cal x y = function
        [] -> "valid"
        | "U"::tail when y + 1 <= Y -> cal x (y + 1) tail
        | "R"::tail when x + 1 <= X -> cal (x + 1) y tail
        | "D"::tail when y - 1 >= 1 -> cal x (y - 1) tail
        | "L"::tail when x - 1 >= 1 -> cal (x - 1) y tail
        | _ -> "invalid"
    [for i in [1 .. n] -> Console.ReadLine().Trim()] 
    |> cal 1 1
    |> Console.WriteLine
    0
