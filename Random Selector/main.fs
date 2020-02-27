open System.IO
open System
open System.Diagnostics
open System.Collections.Generic

let (|>|>) x f = f x |> ignore; x

let rec getAllFiles dir =
    seq {
        yield!  Directory.EnumerateFiles(dir) 
        for d in Directory.EnumerateDirectories(dir) do
            yield! getAllFiles d
    }
    
[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 ->
        let lst = new List<string>()
        let rand = new Random(DateTime.Now.ToString().GetHashCode())
        argv.[0] |> getAllFiles |> Seq.iter (fun x -> lst.Add(x))
        while lst.Count > 0 do
            let i = rand.Next(0, lst.Count)
            let p = lst.[i]
            Console.WriteLine p
            try Process.Start(p).Start() |> ignore with | _ -> ()
            lst.RemoveAt i
            Console.ReadLine() |> ignore
    | _ -> ()
    0
