open System.Diagnostics
open System
open System.IO
open Microsoft.WindowsAPICodePack.Dialogs

let AppPath = AppDomain.CurrentDomain.SetupInformation.ApplicationBase
let DataPath = AppPath + "Data\\"
let BatPath = DataPath + "QiniuDirectoryUpload.bat"
let LogPath = (DataPath + "Log" |> Directory.CreateDirectory).FullName + "\\"
let QshellFilename = "qshell-windows-x64.exe"
let UploadConfigPath = DataPath + "upload.json"

let GetTime _ =
    DateTime.Now.ToLocalTime().ToString("u").Replace(" ", "").Replace("-", "").Replace(":", "").Replace("Z", "")

let SystemRaw (command : string) visible =
    let mutable Output = String.Empty
    let sw = new StreamWriter(BatPath)
    sw.Write command
    sw.Close()
    let mutable StartInfo = new ProcessStartInfo()
    StartInfo.FileName <- BatPath
    StartInfo.Arguments <- "2>&1"
    StartInfo.RedirectStandardError <- true
    StartInfo.RedirectStandardOutput <- true
    StartInfo.UseShellExecute <- false
    StartInfo.WorkingDirectory <- DataPath
    StartInfo.CreateNoWindow <- true
    let cmd = new Process()
    cmd.StartInfo <- StartInfo
    cmd.OutputDataReceived.Add (fun e -> 
        if String.IsNullOrEmpty e.Data <> true then
             if visible then
                if AppPath |> e.Data.Contains = false then
                    "[*] " + e.Data |> Console.WriteLine
             Output <- Output + "\n" + e.Data)
    cmd.EnableRaisingEvents <- true
    cmd.Start() |> ignore
    cmd.BeginOutputReadLine()
    cmd.WaitForExit()
    Output
    
let System command =
    SystemRaw command true

let SystemInvisible command =
    SystemRaw command false

let Qshell command =
    QshellFilename + " " + command |> System

let QshellInvisible command =
    QshellFilename + " " + command |> SystemInvisible

let GetBuckets _ =
    let raw = "buckets" |> QshellInvisible
    (('\n' |> raw.Trim().Split) |> List.ofArray).Tail

let SelectBucket _ =
    "[-] Buckets" |> Console.WriteLine
    let buckets = GetBuckets()
    for i in List.zip ([1 .. GetBuckets() |> List.length] |> List.map (fun x -> x |> Convert.ToString)) (GetBuckets()) do
        "[*] " + (i |> fst) + " | " + (i |> snd) |> Console.WriteLine
    "bucket : " |> Console.Write
    buckets.[(Console.ReadLine() |> Convert.ToInt32) - 1]

    

let Logic accessKey secretKey = 
    if accessKey <> "" && secretKey <> "" then 
        ("account " + accessKey + " " + secretKey) |> Qshell |> ignore

let CheckLogic _ = 
    let res = "buckets" |> QshellInvisible
    match "error" |> res.Contains with
    | true -> false
    | _ -> true
    
let DirectoryPick _ =
    let dialog = new CommonOpenFileDialog()
    dialog.IsFolderPicker <- true
    match dialog.ShowDialog() with
    | CommonFileDialogResult.Ok -> dialog.FileName
    | _ -> String.Empty

let Doublize (input : string) =
    String.Join("\\\\", '\\' |> input.Split)

let Qupload configPath =
    "qupload " + configPath |> Qshell

let Build bucket path =
    let uploadConfig = new StreamWriter(UploadConfigPath)
    uploadConfig.WriteLine("{")
    uploadConfig.WriteLine("   \"src_dir\":\"" + (path |> Doublize) + "\",")
    uploadConfig.WriteLine("   \"bucket\":\"" + bucket + "\",")
    uploadConfig.WriteLine("   \"ignore_dir\":false,")
    uploadConfig.WriteLine("   \"overwrite\":true,")
    uploadConfig.WriteLine("   \"check_exists\":true,")
    uploadConfig.WriteLine("   \"check_hash\":true,")
    uploadConfig.WriteLine("   \"check_size\":false,")
    uploadConfig.WriteLine("   \"rescan_local\":true,")
    uploadConfig.WriteLine("   \"log_file\":\"" + (LogPath |> Doublize) + GetTime()+ ".log\",")
    uploadConfig.WriteLine("   \"log_level\":\"debug\",")
    uploadConfig.WriteLine("   \"log_rotate\":1,")
    uploadConfig.WriteLine("   \"log_stdout\":true,")
    uploadConfig.WriteLine("   \"file_type\":0")
    uploadConfig.WriteLine("}")
    uploadConfig.Close()
    
let QiniuDirectoryUpload (bucket : string) (path : string) =
    if CheckLogic() = true then
        "是否以上次登录的账号继续?(y/n)" |> Console.Write
        if Console.ReadLine() = "n" then
            Logic "1" "1"
    while CheckLogic() = false do
        "[!] Logic Fail" |> Console.WriteLine
        "AccessKey : " |> Console.Write
        let ak = Console.ReadLine()
        "SecretKey : " |> Console.Write
        let sk = Console.ReadLine()
        Logic ak sk
    "[*] Logic Successed" |> Console.WriteLine
    let bucket = SelectBucket()
    let path = DirectoryPick()
    Build bucket path
    "\"" + UploadConfigPath + "\"" |> Qupload

[<STAThread>]
[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> QiniuDirectoryUpload "" ""
    | _ -> QiniuDirectoryUpload argv.[0] argv.[1]
    |> ignore
    Console.ReadLine() |> ignore
    0
