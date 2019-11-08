open System.Net
open System
open System.IO

let _start = "register_resp("
let _end = "); </script>"
let __start = "\"file_url\":\""
let __end = "\""

module Log =
    let printStr line (str : string) = 
        match line with
        | true -> Console.WriteLine str
        | _ ->  Console.Write str
    let print line = printStr line << String.concat " "
    let printColor line color (args : string list) =
        System.Console.ForegroundColor <- color
        print line args
    let Error = printColor false ConsoleColor.Red
    let Log = printColor false ConsoleColor.White
    let Info = printColor false ConsoleColor.DarkGray
    let ErrorLn = printColor true ConsoleColor.Red
    let LogLn = printColor true ConsoleColor.White
    let InfoLn = printColor true ConsoleColor.DarkGray
    let Time _ = "[" + DateTime.Now.ToString() + "]"

let YanderDownloader downloadPath start num logPath databasePath =
    seq{ start .. start + num - 1 }
    |> Seq.iter (fun i ->
        try
            Log.LogLn [Log.Time(); "Start"; i.ToString(); " ... "]
            use wc = new WebClient()
            let json = 
                wc.DownloadString("https://yande.re/post/show/" + i.ToString())
                |> (fun x -> 
                    x.Substring(x.IndexOf(_start) + _start.Length) 
                        |> (fun y -> y.Substring(0, y.IndexOf(_end))))
            Log.InfoLn [json]
            let fileUrl = 
                json.Substring(json.IndexOf(__start)+__start.Length)
                |> (fun x -> x.Substring(0,x.IndexOf(__end)))
            Log.InfoLn [fileUrl]
            let _decoded = WebUtility.UrlDecode << Path.GetFileName <| fileUrl
            let _path = Path.Combine(downloadPath, _decoded)
            match File.Exists _path with
            | false ->
                if String.IsNullOrWhiteSpace databasePath |> not then
                    Log.Log [Log.Time(); "Write to database ... "]
                    use sw = new StreamWriter(databasePath, true)
                    sw.WriteLine json
                    Log.LogLn ["[done]"]
                Log.Log [Log.Time(); "Downloading File ... "]
                let _temp = Path.Combine(Environment.CurrentDirectory, _decoded)
                wc.DownloadFile(fileUrl, _temp)
                wc.Dispose()
                File.Move(_temp, _path)
                Log.LogLn ["[done]"]
            | _ ->()
        with
        | e -> 
            if String.IsNullOrWhiteSpace logPath |> not then
                use sw = new StreamWriter(logPath, true)
                sw.WriteLine(i.ToString() + " " + e.Message)
            Log.ErrorLn [Log.Time(); e.Message])

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 3 -> YanderDownloader (argv.[0]) (Convert.ToInt32(argv.[1])) (Convert.ToInt32(argv.[2])) "" ""
    | 4 -> YanderDownloader (argv.[0]) (Convert.ToInt32(argv.[1])) (Convert.ToInt32(argv.[2])) (argv.[3]) ""
    | 5 -> YanderDownloader (argv.[0]) (Convert.ToInt32(argv.[1])) (Convert.ToInt32(argv.[2])) (argv.[3]) (argv.[4])
    | _ -> Console.WriteLine "DownloadPath Start Num [LogPath] [DatabasePath]"
    0
