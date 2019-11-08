open System.IO
open System.Linq
open System
open Microsoft.WindowsAPICodePack.Taskbar
open System.Drawing
open System.Diagnostics

let (|>|>) x f = f x |> ignore; x

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 ->
        argv.[0]
        |> Directory.GetFiles
        |> (fun x -> x.OrderBy(fun _ -> Guid.NewGuid()).ToList().[0])
        |>|> (fun x -> TaskbarManager.Instance.SetOverlayIcon(Icon.ExtractAssociatedIcon(x), ""))
        |>|> Console.WriteLine
        |>|> (fun _ -> Console.ReadLine())
        |> (fun x -> Process.Start(x).Start())
        |> ignore
    | _ -> ()
    0
