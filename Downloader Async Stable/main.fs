open System
open System.IO
open Microsoft.WindowsAPICodePack.Dialogs
open System.Net

let (|>>) x f = f x; x

module IO =
    let rec Input _ =
        match Console.ReadLine().Trim() with
        | s when s <> "" -> s::Input()
        | _ -> []

    let DirectoryPick _ =
        let dialog = new CommonOpenFileDialog()
        dialog.IsFolderPicker <- true
        match dialog.ShowDialog() with
        | CommonFileDialogResult.Ok -> dialog.FileName
        | _ -> failwith "Empty Path"

module Log =
    open System.Threading

    type Level =
        | Log
        | Error
        | Info

    type Type =
        | ConsoleType of ConsoleColor
        | FileType of string

    let output =
        let lockObj = obj()
        fun logType level (s:string) ->
            lock lockObj (fun _ ->
                let msg = 
                    match level with
                    | Log -> "[*] "
                    | Error -> "[!] "
                    | Info -> "[i] "
                    + s
                match logType with
                | ConsoleType(c) ->
                    Console.ForegroundColor <- c
                    Console.WriteLine msg
                | FileType(p) ->
                    use log = new StreamWriter(p, true)
                    log.WriteLine msg)

    let Log = output (ConsoleType ConsoleColor.White) Log
    let Error = output (ConsoleType ConsoleColor.Red) Error
    let Info = output (ConsoleType ConsoleColor.DarkGray) Info
    let File path = output (FileType path)
    let LogAsync msg = (new Thread(fun () -> Log msg)).Start()
    let ErrorAsync msg = (new Thread(fun () -> Error msg)).Start()
    let InfoAsync msg = (new Thread(fun () -> Info msg)).Start()
    let FileAsync path level msg = (new Thread(fun () -> File path level msg)).Start()
    
let DownloadAsync path lst =
    let tempPath = path + "\\" + "temp"
    Directory.CreateDirectory tempPath |> ignore
    let logPath = path + "\\" + "Error.log"
    lst
    |> Seq.ofList
    |> Seq.map (fun url ->
        async{
            let filename = (Path.GetFileName url).Split('?').[0]
            let downloadPath = path + "\\" + filename
            let tempDownloadPath = tempPath + "\\" + filename
            if not <| File.Exists downloadPath then
                try
                    use wc = new WebClient()
                    wc.DownloadFile(Uri(url), tempDownloadPath)
                    File.Move(tempDownloadPath,downloadPath)
                    url + " -> " + filename + " done." |> Log.InfoAsync
                with 
                | e ->
                    url + " " + e.Message |>> Log.Error |> Log.FileAsync (logPath) Log.Level.Error
        })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

[<STAThread>]
[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> DownloadAsync (IO.DirectoryPick()) (IO.Input())
    | _ -> 
        use sr = new StreamReader(argv.[1])
        sr.ReadToEnd().Split('\n')
        |> List.ofArray
        |> DownloadAsync argv.[0]
    Log.LogAsync "done."
    Console.ReadLine() |> ignore
    0
