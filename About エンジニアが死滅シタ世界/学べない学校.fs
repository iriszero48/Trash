open System
[<EntryPoint>]
let main _ = 
    let rec cal A B = function
        [] -> [A;B]
        | a::b::tail when a = b -> cal A B tail
        | "c"::"p"::tail | "p"::"g"::tail | "g"::"c"::tail -> cal (A + 1) B tail
        | _::_::tail -> cal A (B + 1) tail
        | _ -> failwith "fq"
    [for i in 
        [1 .. Console.ReadLine() |> Convert.ToInt32] do 
            yield! Console.ReadLine().Trim().Split(' ') |> List.ofArray] 
    |> cal 0 0 
    |> List.iter Console.WriteLine
    0
