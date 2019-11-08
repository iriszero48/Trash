open System.Net

[<EntryPoint>]
let main _ = 
    seq{1 .. 35} 
        |> Seq.map (fun x -> 
            async{
                (new WebClient()).DownloadFile(
                    "https://static.event.mihoyo.com/bh3_homepage/images/pic/picture/" + 
                        x.ToString().PadLeft(2, '0') + 
                            ".jpg",x.ToString().PadLeft(2, '0') + ".jpg")
                                |> printfn "%A"}) 
                                    |> Async.Parallel 
                                        |> Async.RunSynchronously 
                                            |> ignore
    0
