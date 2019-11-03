type SeqList<'a> (?items) =
    let items = defaultArg items []
    member this.InsertAfter index item = 
        SeqList((List.chunkBySize index items).Head @ [item] @ List.skip index items)
    member this.RemoveAt index =
        SeqList((List.chunkBySize index items).Head @ List.skip index items) 
    member this.Items =
        items

[<EntryPoint>]
let main _ =
    let s = new SeqList<int>([654;54;64;78;2;2])
    printfn "%A" s.Items
    printfn "%A" (s.InsertAfter 1 233).Items
    printfn "%A" (s.RemoveAt 2).Items
    System.Console.ReadLine() |> ignore
    0
