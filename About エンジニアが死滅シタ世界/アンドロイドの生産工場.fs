open System
[<EntryPoint>]
let main _ = 
    Console.ReadLine().ToCharArray() 
    |> Array.mapi (fun i x -> 
        match i % 2 with
        | 0 -> x.ToString()
        | _ -> "")
    |> Array.reduce (+)
    |> Console.WriteLine
    0
