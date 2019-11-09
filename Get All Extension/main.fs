open System.IO
open System

let rec getAllFiles dir =
    seq { yield! try 
                    Directory.EnumerateFiles(dir, "*.*") 
                 with 
                 | _ -> Seq.empty
          for d in (try 
                    Directory.EnumerateDirectories(dir) 
                    with 
                    | _ -> Seq.empty) do
              yield! getAllFiles d }

let GetAllExtension path =
    path
    |> getAllFiles
    |> Seq.map Path.GetExtension
    |> Seq.distinct
    |> Seq.iter Console.WriteLine

[<EntryPoint>]
let main argv = 
    GetAllExtension argv.[0]
    0
