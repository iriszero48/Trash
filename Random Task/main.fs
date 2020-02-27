open System
open System.IO

[<EntryPoint>]
let main _ = 
    let rand = new Random(DateTime.Now.ToString().GetHashCode())
    let lst = File.ReadAllLines("task.txt")
    Console.WriteLine(lst.[rand.Next(0, lst.Length)])
    Console.ReadLine() |> ignore
    0
