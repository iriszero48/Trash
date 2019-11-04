open System.IO
open System
open System.Diagnostics

let szPath = @"D:\Program Files\7-Zip\7z.exe"
let rootPath = @"Z:\bk"
let DFpath = @"D:\Project\iriszeri-hayate.visualstudio.com\Directory Flattener\Directory Flattener\Directory Flattener\bin\Release\Directory_Flattener.exe"

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

let LogRaw (e : string) = LogSystem.Post(ConsoleColor.DarkGray, e)
let Log (e : string) = LogSystem.Post(ConsoleColor.White,"[*] " + e)
let LogInfo (e : string) = LogSystem.Post(ConsoleColor.DarkGray,"[i] " + e)
let LogError (e : string) = LogSystem.Post(ConsoleColor.Red,"[!] " + e)
let (|>>) x f = f x; x
let Run = Async.Parallel >> Async.RunSynchronously >> ignore
let DoubleQuotes i = "\"" + i + "\""
let Argsize args = args |> String.concat " "
let LogInfoArgs = Argsize >> LogInfo
let LogInfoWithReturn e = e |>> LogInfo

let Caller path args rootPath =
    let mutable StartInfo = new ProcessStartInfo()
    StartInfo.FileName <- path
    StartInfo.Arguments <- args
    StartInfo.RedirectStandardError <- true
    StartInfo.RedirectStandardOutput <- true
    StartInfo.UseShellExecute <- false
    StartInfo.WorkingDirectory <- rootPath
    StartInfo.CreateNoWindow <- true
    let cmd = new Process()
    cmd.StartInfo <- StartInfo
    cmd.OutputDataReceived.Add (fun e -> 
        match e.Data with
        | msg when String.IsNullOrEmpty msg |> not -> LogRaw msg
        | _ -> ())
    cmd.EnableRaisingEvents <- true
    cmd.Start() |> ignore
    cmd.BeginOutputReadLine()
    cmd.WaitForExit()

let ServeZip args = Caller szPath args rootPath
let DirectoryFlattener args = Caller DFpath args rootPath

let KoishiViewer _ =
    Directory.EnumerateDirectories(rootPath)
    |> Seq.map 
        (fun m -> 
            async{
                let exPath = Path.Combine(m, "ex")
                Directory.CreateDirectory exPath |> ignore
                Log <| "move to " + exPath
                Directory.EnumerateDirectories m
                |> Seq.map 
                    (fun p ->
                        async{
                            match Path.GetFileName(p) with
                            | "ex" -> ()
                            | name -> 
                                let psPath = Path.Combine(m,"ex",name)
                                ["move"; psPath; "to"; p] |> LogInfoArgs
                                Directory.Move(p,psPath)
                        })
                |> Run
                Log <| "Directory Flatten " + exPath
                DirectoryFlattener exPath
                Log <| "remove ps dir"
                Directory.EnumerateDirectories(exPath)
                |> Seq.iter (LogInfoWithReturn >> Directory.Delete)
                Log <| "unzip " + m
                Directory.EnumerateFiles m
                |> Seq.iter 
                    (fun x ->
                        if Path.GetExtension x = ".zip" then
                            LogInfo <| "unzip " + x 
                            ["x";
                            DoubleQuotes x;
                            "-o" + DoubleQuotes  
                                (Path.Combine(exPath,Path.GetFileNameWithoutExtension x))]
                            |> Argsize
                            |> ServeZip
                            File.Delete x)
                if Directory.EnumerateFileSystemEntries exPath |> Seq.length = 0 then
                    Directory.Delete(exPath)
            })
    |> Run

[<EntryPoint>]
let main _ = 
    LogSystem.Start()
    KoishiViewer()
    Log "done."
    Console.ReadLine() |> ignore
    0
