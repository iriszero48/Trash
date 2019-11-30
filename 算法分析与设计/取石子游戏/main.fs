open System

let GoldenRatio = (sqrt 5. + 1.) / 2.

let WythoffsGame m n =
    let a::b::_ = List.sort [m; n]
    if (GoldenRatio * float (b - a)).ToString().Split('.').[0] |> Int32.Parse = a then false else true

[<EntryPoint>]
let main _ = 
    while true do 
        match Console.ReadLine() with
        | null -> exit 0
        | str ->
            let d = str.Split ' ' |> Array.map Int32.Parse
            WythoffsGame d.[0] d.[1] |> printfn "%A"
    0
