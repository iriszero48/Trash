open System.Net
open System
open System.IO
open System.Threading

module Log =
    let lock = new System.Threading.SemaphoreSlim(1, 1)
    let printStr line (str : string) =
        lock.Wait()
        match line with
        | true -> Console.WriteLine str
        | _ ->  Console.Write str
        lock.Release() |> ignore
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
    
let rec AutoIndexDownloaderAsync logPath threadNum (url:string) path =
    let lock = new SemaphoreSlim(threadNum, threadNum)
    let rec AutoIndexDownloader logPath (url:string) path = 
        try
            use wc = new WebClient()
            wc.DownloadString(url)
                .Split([|"<a href=\""|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.ofArray
            |> Seq.tail
            |> Seq.map (fun x -> x.Substring(0,x.IndexOf("\"")))
            |> Seq.filter (fun x -> 
                not <| x.StartsWith("?") && 
                not <| x.StartsWith("..") &&
                not <| x.Equals "/" &&
                not <| url.Substring(0, url.Substring(0, url.Length-1).LastIndexOf("/") + 1).EndsWith(x))
            |> Seq.map (fun x ->
                async {
                    lock.Wait()
                    Log.Info [x]
                    match x.EndsWith("/") with
                    | true ->
                        AutoIndexDownloader logPath (url + x) 
                        <| Directory.CreateDirectory(
                            Path.Combine(
                                path,
                                WebUtility.UrlDecode(x.Substring(0,x.Length-1)).Replace('|','l')))
                           .FullName
                    | _ ->
                        let _decoded = WebUtility.UrlDecode(x).Replace('|','l')
                        let _path = Path.Combine(path, _decoded)
                        match File.Exists _path with
                        | false ->
                            try
                                let _url = url + x
                                let _temp = Path.Combine(Environment.CurrentDirectory, _decoded)
                                Log.Log [_url; "=>"; _path]
                                use wc = new WebClient()
                                wc.DownloadFile(_url, _temp)
                                File.Move(_temp, _path)
                            with
                            | e ->
                                if not <| String.IsNullOrWhiteSpace logPath then
                                    use sw = new StreamWriter(logPath, true)
                                    sw.WriteLine(x.ToString() + " " + e.Message)
                                Log.Error [e.Message]
                        | _ -> ()
                    lock.Release() |> ignore
                })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
        with
        | e ->
            if not <| String.IsNullOrWhiteSpace logPath then
                use sw = new StreamWriter(logPath, true)
                sw.WriteLine(url + " " + e.Message)
            Log.Error [e.Message]
    AutoIndexDownloader logPath url path

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 3 -> AutoIndexDownloaderAsync "" (Convert.ToInt32(argv.[2])) (argv.[0]) (argv.[1])
    | 4 -> AutoIndexDownloaderAsync (argv.[3]) (Convert.ToInt32(argv.[2])) (argv.[0]) (argv.[1])
    | _ -> Console.WriteLine "Url Path threadNum [LogPath]"
    0
