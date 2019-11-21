let rec QuickSortAsync lst = 
    match lst with
    | h::t ->
        let l, r = List.partition ((>=) h) t
        let task = 
            [QuickSortAsync l; QuickSortAsync r]
            |> Async.Parallel
            |> Async.RunSynchronously
        async{ return (task.[0]) @ (h::(task.[1])) }
    | _ -> async{ return [] }
