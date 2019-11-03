open System
[<EntryPoint>]
let main _ = 
    let rec cal1 s t w d l = function
        [] -> [s; t; w; d; l]
        | 'W'::tail -> cal1 s (t + 2) (w + 1) d l tail
        | 'D'::tail -> cal1 s (t + 1) w (d + 1) l tail
        | 'L'::tail -> cal1 s t w d (l + 1) tail
        | _::tail -> cal1 s t w d l tail
    let rec cal2 s = function
        [] -> []
        | head::tail -> (cal1 s 0 0 0 0 head)::cal2 (s + 1) tail
    [for _ in 
        [1 .. Console.ReadLine() |> Convert.ToInt32] -> 
            Console.ReadLine().Trim().ToCharArray() |> List.ofArray]
    |> cal2 1
    |> List.maxBy (fun lst -> lst.[1])
    |> List.map Convert.ToString
    |> String.concat " "
    |> Console.WriteLine
    0
