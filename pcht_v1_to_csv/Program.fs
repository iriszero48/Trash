open System
open System.IO
open System.Text

open FSharp.Data

let base_path = @""
let output_path = Path.Combine(base_path, "pcht-v1.csv")

type output_data_type = CsvProvider<
    Schema="ID (int64), PASSWORD (string), MD5 (string), SHA1 (string), NTLM (string), MYSQL (string), SHA256 (string)",
    HasHeaders=false,
    CacheRows=false>

let run _ =
    let raw = 
        seq {
            for f in Directory.EnumerateFiles(base_path, "*.txt") do
            for l in File.ReadLines(f, Encoding.ASCII) -> l }
        |> Seq.map (fun l -> l.Split("|") |> Seq.toArray)

    let dat = seq {
        for rows in raw do
            match Array.length rows with
            | 7 -> yield output_data_type.Row(Convert.ToInt64 (rows.[0]), rows.[1], rows.[2], rows.[3], rows.[4], rows.[5], rows.[6])
            | len when len > 7 ->
                let password = String.Join("|", rows.[1..(len - 6)])
                yield output_data_type.Row(Convert.ToInt64 (rows.[0]), password, rows.[len - 5], rows.[len - 4], rows.[len - 3], rows.[len - 2], rows.[len - 1])
            | 6 ->
                printfn "Error 6: %A" rows
                yield output_data_type.Row(Convert.ToInt64 (rows.[0]), rows.[1].Substring(0, rows.[1].Length - 32 - 1), rows.[1].Substring(rows.[1].Length - 32), rows.[2], rows.[3], rows.[4], rows.[5])
            | _ -> printfn "Unexpected row length: %d, data = %A" (Array.length rows) rows
    }

    (new output_data_type(dat)).Save(output_path)

[<EntryPoint>]
let main _ =
    run()
    printfn "Done."
    Console.ReadLine() |> ignore
    0
