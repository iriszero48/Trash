open System.Text.Json
open System.IO
open System.Collections.Generic
open System.Text
open System
open System.Text.Encodings.Web
open System.Text.Unicode

[<EntryPoint>]
let main args =
    match args.Length with
    | 2 ->
        let prefix = args[0]
        let raw_path = args[1]

        let key = $"{prefix}_原始值"

        for line in File.ReadLines(raw_path, Encoding.UTF8) do
            if String.IsNullOrEmpty line |> not then
                let dict = new Dictionary<string, string>()
                dict.Add(key, line)
                let opt = JsonSerializerOptions()
                opt.Encoder <- JavaScriptEncoder.Create(UnicodeRanges.All)
                Console.WriteLine(JsonSerializer.Serialize(dict, opt))

        0
    | _ ->
        eprintfn "args.Length <> 2 which args.Length = %d" args.Length
        printfn "Usage: %s prefix path" AppDomain.CurrentDomain.FriendlyName
        1
