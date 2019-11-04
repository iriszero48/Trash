open System
open System.IO
open Microsoft.WindowsAPICodePack.Dialogs
open System.Net
open System.Threading
open Microsoft.WindowsAPICodePack.Taskbar
open System.Drawing

let UserAgent = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0"
let Referer = "https://www.pixiv.net/member_illust.php?mode="
let KoishiPath = @"K:\z 181009-181202\新建文件夹 (51)"

let (|>>) x f = f x |> ignore ; x

let Log (e : string) =
    Console.ForegroundColor <- ConsoleColor.White
    Console.WriteLine e

let LogInfo (e : string) =
    Console.ForegroundColor <- ConsoleColor.Blue
    Console.WriteLine e

let LogError (e : string) =
    Console.ForegroundColor <- ConsoleColor.Red
    Console.WriteLine e

let LogFile path (e : string) =
    let sw = new StreamWriter(path, true)
    sw.WriteLine e
    sw.Close()

let PathSeparator = Path.DirectorySeparatorChar.ToString()

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
        TaskbarManager.Instance.SetProgressState(TaskbarProgressBarState.Error)
        (" |  " + Path.GetFileName url.url + " " + e.Message) 
        |>> LogError
        |> LogFile (directory + PathSeparator + "Error.log")

let PixivUrlsDownload rawUrls =
    TaskbarManager.Instance.SetProgressState(TaskbarProgressBarState.Indeterminate)
    match rawUrls |> List.length with
    | 0 -> ()
    | _ ->
        let directory = 
            Directory.CreateDirectory(KoishiPath + "\\" + 
                ((((new DirectoryInfo(KoishiPath)).GetDirectories() 
                |> Array.map (fun i -> Convert.ToInt32 i.Name)
                |> Array.max) + 1).ToString())).FullName
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
            PixivUrlDownload directory u i rawUrls.Length
            TaskbarManager.Instance.SetProgressValue(i + 1, rawUrls.Length))
    TaskbarManager.Instance.SetProgressState(TaskbarProgressBarState.NoProgress)

[<STAThread>]
[<EntryPoint>]
let main _ = 
    while true do
        Input() |> PixivUrlsDownload
        Log "Done."
    0
