type Queue<'T> (?items) =
    let items = defaultArg items []
    member this.Items = items
    member this.Enqueue(A) = 
        Queue(items@[A])
    member this.Dequeue() = 
        match items with
        | head::tail -> (head,Queue(tail))
        | _ -> failwith "Null"

[<EntryPoint>]
let main _ =
    let q1 = Queue([1;2;3;4;5])
    let q2 = q1.Enqueue 6
    let i,q3 = q2.Dequeue()
    printfn "%A" q1.Items
    printfn "%A" q2.Items
    printfn "%A" (i,q3.Items)
    System.Console.ReadLine() |> ignore
    0
