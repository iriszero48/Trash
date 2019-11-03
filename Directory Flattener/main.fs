open System
open Microsoft.WindowsAPICodePack.Dialogs
open System.IO

let (|>>) x f = f x ; x

let Print color (e : string) =
    lock (ref Console.ForegroundColor) (fun _ -> 
        Console.ForegroundColor <- color;
        Console.WriteLine e)

let LogInfo (e : string) =
    "[i] " + e |> Print ConsoleColor.DarkGray

let LogError (e : string) =
    "[!] " + e |> Print ConsoleColor.Red

let DirectoryPick _ =
    let dialog = new CommonOpenFileDialog()
    dialog.IsFolderPicker <- true
    match dialog.ShowDialog() with
    | CommonFileDialogResult.Ok -> dialog.FileName
    | _ -> failwith "Empty Path"

let rec getAllFiles dir =
    seq { 
        yield! 
            try 
                Directory.EnumerateFiles(dir, "*.*") 
            with 
            | _ -> Seq.empty
        for d in 
            (try 
                Directory.EnumerateDirectories(dir) 
             with 
             | _ -> Seq.empty) do
                yield! getAllFiles d 
    }

let DirectoryFlattener path file = 
    async{
        try
            let des = Path.Combine(path, Path.GetFileName file)
            if File.Exists des && file <> des then File.Delete des
            File.Move((file |>> LogInfo),des)
        with
        | e -> LogError e.Message
    }

let DirectoryFlattenerAsync path =
    path
    |> getAllFiles
    |> Seq.map (fun f -> DirectoryFlattener path f)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    

[<STAThread>]
[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 0 -> DirectoryPick()
    | _ -> argv.[0]
    |> DirectoryFlattenerAsync
    0
