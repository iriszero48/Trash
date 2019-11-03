open System
[<EntryPoint>]
let main _ = 
    let input _ = Console.ReadLine() |> Convert.ToInt32
    match input() - input() with
    | x when x >= 0 -> x.ToString()
    | _ -> "error"
    |>Console.WriteLine
    0
