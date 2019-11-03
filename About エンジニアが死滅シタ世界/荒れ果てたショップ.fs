open System
[<EntryPoint>]
let main _ = 
    let input = Console.ReadLine().Split(' ') |> Array.map Convert.ToInt32
    [input.[1] .. input.[2]] 
    |> List.iter (fun x -> 
        x.ToString().PadLeft(input.[0], '0') 
        |> Console.WriteLine)
    0
