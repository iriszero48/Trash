open System.IO
open System.Linq
open System
open Microsoft.WindowsAPICodePack.Taskbar
open System.Drawing
open System.Diagnostics
open System.Security.Cryptography

let (|>|>) x f = f x |> ignore; x

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 ->
        let rng = new RNGCryptoServiceProvider()
        let arr = Array.create 8 0uy
        argv.[0]
        |> Directory.GetFiles
        |> (fun x -> x.OrderBy(fun _ -> rng.GetBytes(arr); BitConverter.ToUInt64(arr, 0)).ToList().[0])
        |>|> (fun x -> TaskbarManager.Instance.SetOverlayIcon(Icon.ExtractAssociatedIcon(x), ""))
        |>|> Console.WriteLine
        |>|> (fun _ -> Console.ReadLine())
        |> (fun x -> Process.Start(x).Start())
        |> ignore
    | _ -> ()
    0
