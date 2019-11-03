open System
open System.Windows.Forms 

let webDocumentCompleted (sender: obj) (e: WebBrowserDocumentCompletedEventArgs) =
    let web = sender :?> (WebBrowser) 
    Console.Write(web.DocumentText)

[<STAThread>]
[<EntryPoint>]
let main argv= 
    let url =
        match argv.Length with
        | 0 -> Console.ReadLine()
        | _ -> argv.[0]
    let web = new WebBrowser()
    web.Navigate(url)
    web.DocumentCompleted.Add(webDocumentCompleted web)
    while web.ReadyState <> WebBrowserReadyState.Complete do               
        Application.DoEvents()
    Console.Read() |> ignore
    0
