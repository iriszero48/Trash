open System
Console.ReadLine() |> ignore
Console.ReadLine().Split(' ') 
|> Array.filter (fun x -> 
    Convert.ToInt32 x > 5) 
|> Array.length 
|> Console.WriteLine
