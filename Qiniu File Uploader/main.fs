open System.Diagnostics
open System
open System.IO
open System.Windows.Forms

let AppPath = AppDomain.CurrentDomain.SetupInformation.ApplicationBase
let DataPath = AppPath + "Data\\"
let BatPath = DataPath + "QiniuDirectoryUpload.bat"
let QshellFilename = "qshell-windows-x64.exe"

let Input (tips : string) =
    Console.Write tips
    Console.ReadLine()

let (|>>) x f = f x; x

let DoubleQuotes i =
    "\"" + i + "\""

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
                if not <| e.Data.Contains AppPath then
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
    (('\n' 
    |> raw.Trim().Split) 
    |> List.ofArray).Tail

let SelectBucket _ =
    "[-] Buckets" |> Console.WriteLine
    (GetBuckets()
    |>> List.iteri (fun i x ->
        ["[*]"; i.ToString(); "|"; x] 
        |> String.concat " " 
        |> Console.WriteLine)).[("bucket:" 
    |> Input 
    |> Convert.ToInt32) - 1]

let Logic accessKey secretKey =
    ("account " + accessKey + " " + secretKey) 
    |> Qshell 
    |> ignore

let CheckLogic _ = 
    not <| (QshellInvisible "buckets").Contains "error"
    
let FilePick _ =
    let dialog = new OpenFileDialog()
    match dialog.ShowDialog() with
    | DialogResult.OK -> dialog.FileName
    | _ -> failwith "Empty Path"

let Rput bucket filename path =
    ["rput"; bucket; DoubleQuotes filename; DoubleQuotes path; "--overwrite"] 
    |> String.concat " " 
    |> Qshell 
    |> ignore
    
let QiniuFileUpload _ =
    match CheckLogic() with
    | true ->
        if Input "是否以上次登录的账号继续?(Y/n)" = "n" then
            Logic "0" "0"
    | _ -> 
        while CheckLogic() do
            Console.WriteLine "[!] Logic Fail"
            Logic (Input "AccessKey:") (Input "SecretKey:")
    Console.WriteLine "[*] Logic Successed"
    let path = FilePick()
    let filename = Path.GetFileName path
    Rput (SelectBucket()) (
        match ["filename(default="; DoubleQuotes filename; "):"] 
            |> List.reduce (+) 
            |> Input with 
        | "" -> filename 
        | fname -> fname) path

[<STAThread>]
[<EntryPoint>]
let main _ =
    QiniuFileUpload()
    "Done." 
    |> Input 
    |> ignore
    0
