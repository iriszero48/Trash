open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.IO
open System.Threading

open Downloader

open FSharpUtils

[<Struct>]
type Setting =
    { root_dir: string
      start_path: string
      download_chunk_count: int
      proxy: string }

[<Struct>]
type ListData =
    { path: string
      password: string
      page: int
      per_page: int
      refresh: bool }

[<Struct>]
type GetData = { path: string; password: string }

[<Struct>]
type Item =
    { name: string
      size: int64
      is_dir: bool }

module fuck_alist =
    let fuck_base = "http://www.123114514.xyz"
    let get_api = $"{fuck_base}/api/fs/get"
    let list_api = $"{fuck_base}/api/fs/list"

    let base_api url cur_path data =
        use client = new HttpClient()
        let msg = new HttpRequestMessage()
        msg.Method <- HttpMethod.Post
        msg.RequestUri <- Uri(url)
        msg.Headers.Add("origin", fuck_base)
        msg.Headers.Add(HttpRequestHeader.Referer.ToString(), $"{fuck_base}{cur_path}")

        msg.Headers.Add(
            "user-agent",
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"
        )

        let data_ctx = new StringContent(FuJson.Dump data)
        let ctx = new MediaTypeHeaderValue("application/json")
        ctx.CharSet <- "utf-8"
        data_ctx.Headers.ContentType <- ctx
        msg.Content <- data_ctx
        let resp = client.Send(msg)
        resp.EnsureSuccessStatusCode() |> ignore

        let ret =
            resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously

        Thread.Sleep 1000
        ret

    let list cur_path =
        let data =
            { path = cur_path
              password = ""
              page = 1
              per_page = 0
              refresh = false }

        let js = base_api list_api cur_path data |> FuJson.Load |> FuJson.Get "data"
        printfn "%s" <| FuJson.Pretty js

        if js.ValueKind = Text.Json.JsonValueKind.Null then
            eprintfn "[list] fucking null: %A" cur_path
            None
        else
            Some(js |> (FuJson.Get "content" >> FuJson.AsArray) |> Seq.map FuJson.As<Item>)

    let get cur_path =
        let data = { path = cur_path; password = "" }

        let js = base_api get_api cur_path data |> FuJson.Load |> FuJson.Get "data"
        printfn "%s" <| FuJson.Pretty js

        if js.ValueKind = Text.Json.JsonValueKind.Null then
            eprintfn "[get] fucking null: %A" cur_path
            None
        else
            Some(js |> (FuJson.Get "raw_url" >> FuJson.AsString))

let url_combine ([<ParamArray>] urls) =
    Array.reduce
        (fun s (x: string) ->
            match s.EndsWith('/'), x.StartsWith('/') with
            | true, true -> s + (x.TrimStart('/'))
            | false, false -> s + "/" + x
            | _ -> s + x)
        urls

let rec walk (setting: Setting) cur_path =
    printfn "cd %A" cur_path

    match fuck_alist.list cur_path with
    | None -> ()
    | Some(ls) ->
        for i in ls do
            printfn "is_dir %A -> %A" i.name i.is_dir

            if i.is_dir then
                walk setting (url_combine [| cur_path; i.name |])
            else
                let dir = Path.Combine(setting.root_dir, cur_path.TrimStart('/'))

                if not <| Directory.Exists(dir) then
                    Directory.CreateDirectory(dir) |> ignore

                let fp = Path.Combine(dir, i.name)
                let chk_exist = File.Exists fp

                let chk_size =
                    if chk_exist then
                        FuFileSystem.FileSize fp = i.size
                    else
                        false

                match chk_exist, chk_size with
                | true, true -> "ok."
                | true, false -> sprintf "assert(%A = %A)" (FuFileSystem.FileSize fp) i.size
                | _ -> "failed"
                |> printfn "lsattr %A %s" fp

                if not chk_size then
                    let rec download count =
                        if count <> 0 then
                            let sleep = count * 1000
                            printfn "retrying(%A), sleep %A" count sleep
                            Thread.Sleep sleep

                        let file_url = url_combine [| cur_path; i.name |]

                        try
                            printfn "fetch %A" file_url
                            let down_url = fuck_alist.get file_url

                            match down_url with
                            | None -> ()
                            | Some(down_url) ->
                                printfn "wget %A" down_url
                                let cfg = DownloadConfiguration()
                                cfg.ChunkCount <- setting.download_chunk_count
                                cfg.ParallelDownload <- true

                                if file_url.StartsWith fuck_alist.fuck_base |> not then
                                    if String.IsNullOrWhiteSpace setting.proxy |> not then
                                        let req_cfg = RequestConfiguration()
                                        let proxy = WebProxy()
                                        proxy.Address <- new Uri(setting.proxy)

                                        req_cfg.Proxy <- proxy
                                        cfg.RequestConfiguration <- req_cfg

                                use ds = new DownloadService(cfg)
                                let mutable has_error = false

                                ds.DownloadFileCompleted.Add(fun e ->
                                    if e.Cancelled then
                                        printfn "CANCELED."
                                    elif e.Error |> isNull |> not then
                                        printfn "ERROR. %A" e.Error
                                        has_error <- true
                                    else
                                        printfn "DONE.")

                                let logger =
                                    new MailboxProcessor<string>(fun x ->
                                        let rec loop _ =
                                            async {
                                                let! msg = x.Receive()
                                                printf "%s" msg
                                                return! loop ()
                                            }

                                        loop ())

                                logger.Start()

                                let log_file_size x =
                                    let v, s =
                                        match float x with
                                        | x when x > 1024. ** 3 -> (x / 1024. ** 3), "gb"
                                        | x when x > 1024. ** 2 -> (x / 1024. ** 2), "mb"
                                        | x when x > 1024. ** 1 -> (x / 1024. ** 1), "kb"
                                        | x -> x, "b"

                                    sprintf "%.1f %s" v s

                                ds.DownloadProgressChanged.Add(fun e ->
                                    logger.Post
                                    <| sprintf
                                        "\r%s/s(avg), %s/s, %s/%s, %.1f%%                   "
                                        (log_file_size e.AverageBytesPerSecondSpeed)
                                        (log_file_size e.BytesPerSecondSpeed)
                                        (log_file_size <| float (e.ReceivedBytesSize))
                                        (log_file_size <| float (e.TotalBytesToReceive))
                                        e.ProgressPercentage)

                                let tmp = Path.Combine(Path.GetFullPath("."), i.name)

                                ds.DownloadFileTaskAsync(down_url, tmp)
                                |> Async.AwaitTask
                                |> Async.RunSynchronously

                                if has_error then
                                    failwith "download failed."

                                let tmp_size = FuFileSystem.FileSize tmp

                                if i.size <> tmp_size then
                                    failwith <| sprintf "failwith %A <> %A" i.size tmp_size

                                printfn "mv %A %A" tmp fp
                                File.Move(tmp, fp)
                        with e ->
                            printfn "%A" e
                            download (count + 1)

                    download 0

let run setting = walk setting setting.start_path

[<EntryPoint>]
let main _ =
    let setting_path = "setting.json"

    if File.Exists setting_path |> not then
        let setting =
            { root_dir = Path.GetFullPath(".")
              start_path = "/"
              download_chunk_count = 4
              proxy = "" }

        File.WriteAllText(setting_path, FuJson.Dump setting)

    let setting = File.ReadAllText setting_path |> FuJson.Load |> FuJson.As<Setting>
    printfn "cat %A" setting_path
    printfn "%s" <| FuJson.Pretty setting
    run setting
    printfn "Done."
    Console.ReadLine() |> ignore
    0
