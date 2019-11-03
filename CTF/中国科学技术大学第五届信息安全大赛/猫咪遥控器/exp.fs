open System.IO
open System.Drawing
open System.Drawing.Imaging
open System.Diagnostics
open System
open System.Windows.Forms
open System.Windows

let ReadFile _ =
    let dlg = new OpenFileDialog()
    dlg.ShowDialog() |> ignore
    let sr = new StreamReader(dlg.FileName)
    let res = sr.ReadToEnd()
    sr.Close()
    res

let Draw path =
    let bitmap = new Bitmap(1000,1000)
    let mutable x = 0
    let mutable y = 0
    for i in path do
        match i with
        | 'U' -> Some(y <- y - 1)
        | 'D' -> Some(y <- y + 1)
        | 'L' -> Some(x <- x - 1)
        | 'R' -> Some(x <- x + 1)
        | _ -> None
        |> ignore
        bitmap.SetPixel(x,y,Color.White)
    bitmap.Save("temp.jpg", ImageFormat.Jpeg);

[<EntryPoint>]
let main _ =
    ReadFile() |> Draw
    Process.Start("temp.jpg") |> ignore
    0
