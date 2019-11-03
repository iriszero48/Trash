open System
open System.IO
open Microsoft.WindowsAPICodePack.Dialogs

let (|>>) x f = f x ; x

let DirectoryPick _ =
    let dialog = new CommonOpenFileDialog()
    dialog.IsFolderPicker <- true
    match dialog.ShowDialog() with
    | CommonFileDialogResult.Ok -> dialog.FileName
    | _ -> String.Empty

let rec getAllFiles dir =
    seq { yield! try 
                    Directory.EnumerateFiles(dir, "*.*") 
                 with 
                 | _ -> Seq.empty
          for d in (try 
                    Directory.EnumerateDirectories(dir) 
                    with 
                    | _ -> Seq.empty) do
              yield! getAllFiles d }

let DirectoryWalk path =
    path
    |> getAllFiles 
    |> Seq.map ( fun x -> 
        (x + "    " 
        |>> Console.Write) 
        |> (fun y -> 
            try 
                (new FileInfo(y)).Length 
                |>> Console.WriteLine 
            with 
            | _ -> 0L)) 
        |> Seq.reduce (+) 
        |> (fun x -> 
            (((double)x / 1024. / 1024. / 1024.) 
            |> Convert.ToString) + " GB" 
            |> Console.WriteLine)
    Console.ReadLine() |> ignore

[<STAThread>]
[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> DirectoryPick()
    | _ -> argv.[0]
    |> DirectoryWalk
    0
