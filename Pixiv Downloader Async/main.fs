open System
open System.IO
open Microsoft.WindowsAPICodePack.Dialogs
open System.Net

let UserAgent = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0"
let Referer = "https://www.pixiv.net/member_illust.php?mode="

let mutable count = 0

let (|>>) x f = f x |> ignore ; x

let Log (e : string) =
    Console.ForegroundColor <- ConsoleColor.White
    "[*] " + e |> Console.WriteLine

let LogInfo (e : string) =
    Console.ForegroundColor <- ConsoleColor.Blue
    "[i] " + e |> Console.WriteLine

let LogError (e : string) =
    Console.ForegroundColor <- ConsoleColor.Red
    "[!] " + e |> Console.WriteLine

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
            e.ProgressPercentage.ToString() + "% " + url.url |> LogInfo)
        webClient.Headers.Add(HttpRequestHeader.Cookie,"")
        webClient.DownloadFileCompleted.Add (fun e -> 
            match e.Error with
            | null -> 
                "Completed! " + url.url |> Log
            | error -> 
                webClient.CancelAsync()
                Path.GetFileName url.url + " " + error.Message
                |>> LogError
                |> LogFile (directory + PathSeparator + "Error.log"))
        webClient.DownloadFileAsync(Uri(url.url), path)
    }

let PixivUrlsDownloadAsync rawUrls = 
    let directory = DirectoryPick()
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
        PixivUrlDownloadAsync directory u)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

[<STAThread>]
[<EntryPoint>]
let main argv= 
    match argv.Length with
    | 0 -> Input() |> PixivUrlsDownloadAsync
    | _ -> PixivUrlsDownloadAsync [argv.[0]]
    Log "Done."
    Console.Read() |> ignore
    0
