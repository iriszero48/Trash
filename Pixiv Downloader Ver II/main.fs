open System
open System.IO
open Microsoft.WindowsAPICodePack.Dialogs
open System.Net
open System.Threading

let UserAgent = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0"
let Referer = "https://www.pixiv.net/member_illust.php?mode="

let (|>>) x f = f x |> ignore ; x

let LogSystem = 
    new MailboxProcessor<ConsoleColor * string>(fun x ->
        let rec loop count = 
            async{
                let! c,s = x.Receive()
                Console.ForegroundColor <- c
                Console.WriteLine s
                return! loop (count + 1)
            }
        loop 0)

let Log (e : string) =
    LogSystem.Post(ConsoleColor.White,"[*] " + e)

let LogInfo (e : string) =
    LogSystem.Post(ConsoleColor.DarkGray,"[i] " + e)

let LogError (e : string) =
    LogSystem.Post(ConsoleColor.Red,"[!] " + e)

let LogFile path (e : string) =
    let sw = new StreamWriter(path, true)
    sw.WriteLine e
    sw.Close()

let PathSeparator = Path.DirectorySeparatorChar.ToString()

let DirectoryPick _ =
    let dialog = new CommonOpenFileDialog()
    dialog.IsFolderPicker <- true
    match dialog.ShowDialog() with
    | CommonFileDialogResult.Ok -> dialog.FileName
    | _ -> failwith "Empty Path"

type UrlType =
    | Picture
    | Pictures
    | Zip

type Pid = 
    { url : string;
    urlType : UrlType }

let rec Input _ =
    match Console.ReadLine().Trim().ToLower() with
    | "" -> []
    | s -> s::Input()

let PixivUrlDownloadAsync directory url = 
    async {
        let path = 
            match url.urlType with
            | UrlType.Pictures -> 
                (directory + PathSeparator + (Path.GetFileNameWithoutExtension url.url).Split('_').[0] 
                |>> (fun p -> 
                        match Directory.Exists p with
                        | false -> Some(Directory.CreateDirectory p)
                        | _ -> None)) + PathSeparator
            | _ -> directory + PathSeparator
            + Path.GetFileName url.url
        use webClient = new WebClient()
        webClient.Headers.Add("User-Agent",UserAgent)
        webClient.Headers.Add("Referer",Referer + (match url.urlType with
                                                    | UrlType.Pictures -> "manga"
                                                    | _ -> "medium") 
                                                    + "&illust_id=" 
                                                    + Path.GetFileNameWithoutExtension url.url)
        webClient.DownloadProgressChanged.Add (fun e -> 
            Console.WriteLine("         [i] Progress " + e.ProgressPercentage.ToString() + " " + url.url))
        webClient.Headers.Add(HttpRequestHeader.Cookie,"")
        webClient.DownloadFileCompleted.Add (fun e -> 
            match e.Error with
            | null -> 
                Console.WriteLine("[*] Completed " + url.url)
            | error -> 
                webClient.CancelAsync()
                Console.Write("[!] Error ")
                Console.WriteLine(error)
                Thread.Sleep(10000)
                Console.WriteLine("[!] Redownloading " + url.url)
                webClient.DownloadFileAsync(Uri(url.url), path))
        Console.WriteLine("[*] Downloading " + url.url)
        try
            webClient.DownloadFileAsync(Uri(url.url), path)
        with
        | e -> 
            Console.WriteLine(e)
            webClient.CancelAsync()
        return true
    }

let PixivUrlsDownloadAsync rawUrls = 
    rawUrls 
    |> Seq.ofList
    |> Seq.map (fun raw -> 
        {url=raw;
        urlType=
            (match ('/' 
                |> raw.Split 
                |> List.ofArray 
                |> List.rev) with
            | head::_ when head.Split('.').[1] = "zip" -> 
                UrlType.Zip
            | head::_ when (rawUrls 
                |> List.exists (fun ru -> 
                    ("p0","p1") 
                    |> head.Replace 
                    |> ru.Contains)) -> 
                        UrlType.Pictures
            | _ -> UrlType.Picture)})
    |> Seq.map (fun u -> 
        PixivUrlDownloadAsync (DirectoryPick()) u)
    |> Async.Parallel
    |> Async.RunSynchronously

let PixivUrlDownload directory url index length =
    let path = 
        match url.urlType with
        | UrlType.Pictures -> 
            (directory + PathSeparator + (Path.GetFileNameWithoutExtension url.url).Split('_').[0] 
            |>> (fun p -> 
                    match Directory.Exists p with
                    | false -> Some(Directory.CreateDirectory p)
                    | _ -> None)) + PathSeparator
        | _ -> directory + PathSeparator
        + Path.GetFileName url.url
    let webClient = new WebClient()
    webClient.Headers.Add("User-Agent",UserAgent)
    webClient.Headers.Add("Referer",Referer + (match url.urlType with
                                                | UrlType.Pictures -> "manga"
                                                | _ -> "medium") 
                                                + "&illust_id=" 
                                                + Path.GetFileNameWithoutExtension url.url)
    webClient.Headers.Add(HttpRequestHeader.Cookie,"")
    Log("[-] Downloading " + url.url)
    try
        webClient.DownloadFile(Uri(url.url), path)
        LogInfo(" |  " + (index + 1).ToString() + "/" + length.ToString() + " Succeed!")
    with
    | e -> 
        (" |  " + Path.GetFileName url.url + " " + e.Message) 
        |>> LogError
        |> LogFile (directory + PathSeparator + "Error.log")

let PixivUrlsDownload rawUrls =
    let directory = DirectoryPick()
    rawUrls 
    |> List.map (fun raw -> 
        {url=raw;
        urlType=
            (match ('/' 
                |> raw.Split 
                |> List.ofArray 
                |> List.rev) with
            | head::_ when head.Split('.').[1] = "zip" -> 
                UrlType.Zip
            | head::_ when (rawUrls 
                |> List.exists (fun ru -> 
                    ("p0","p1") 
                    |> head.Replace 
                    |> ru.Contains)) -> 
                        UrlType.Pictures
            | _ -> UrlType.Picture)})
    |> List.iteri (fun i u ->
        PixivUrlDownload directory u i rawUrls.Length)

[<STAThread>]
[<EntryPoint>]
let main argv= 
    LogSystem.Start()
    match argv.Length with
    | 0 -> Input() |> PixivUrlsDownload
    | _ -> PixivUrlsDownload [argv.[0]]
    Log "Done."
    Console.Read() |> ignore
    0
