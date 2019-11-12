open System
open System.IO
open System.Linq

[<EntryPoint>]
let main argv = 
    "task.txt"
    |> File.ReadAllLines
    |> (fun x -> x.OrderBy(fun _ -> Guid.NewGuid()).ToList().First())
    |> Console.WriteLine
    Console.ReadLine() |> ignore
    0
