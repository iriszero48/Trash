open System
[<EntryPoint>]
let main _ = 
    Console.ReadLine().Split(' ') |> Array.sumBy Convert.ToInt32 |> (fun x -> x % 10) |> Console.WriteLine
    0
