type Stack<'a> (?items) =
    let items = defaultArg items []
    member x.Items = items
    member x.Push(A) = 
        Stack(A::items)
    member x.Pop() = 
        match items with
        | head::tail -> (head,Stack(tail))
        | _ -> failwith "Null"

[<EntryPoint>]
let main _ =
    let s1 = Stack([5;4;3;2;1])
    let s2 = s1.Push(6)
    let i,s3 = s2.Pop()
    printfn "%A" s1.Items
    printfn "%A" s2.Items
    printfn "%A" (i,s3.Items)
    System.Console.ReadLine() |> ignore
    0
