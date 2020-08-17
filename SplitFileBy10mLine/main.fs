open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let path = "fmd.csv"
    File.ReadAllLines(path, Encoding.UTF8)
    |> Seq.chunkBySize 10000000
    |> Seq.iteri (fun i x -> File.WriteAllLines(path + "." + i.ToString(), x, Encoding.UTF8))
    0
