open System.Net

[<EntryPoint>]
let main argv =
    while true do
        (new WebClient()).DownloadString(argv.[0]) |> ignore
    0
