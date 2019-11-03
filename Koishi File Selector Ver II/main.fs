open System.IO
open System.Windows.Forms
open System

let FileSelect _ =
    let dialog = new OpenFileDialog()
    match dialog.ShowDialog() with
    | DialogResult.OK -> dialog.FileName
    | _ -> ""

let ReadFile (file : string) =
    let file = new StreamReader(file)
    file.ReadToEnd().Split('\n') 
    |> Array.filter (fun x -> 
        x <> "")

let KoishiFileSelector path =
    FileSelect() 
    |> ReadFile 
    |> Array.map (fun x -> 
        x.Split('\"').[1]) 
    |> Array.iter (fun x -> 
        File.Move("Z:\\"+x,"Z:\\1\\"+x))

[<STAThread>]
[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 0 -> FileSelect() |> KoishiFileSelector
    | _ -> KoishiFileSelector argv.[0]
    0
