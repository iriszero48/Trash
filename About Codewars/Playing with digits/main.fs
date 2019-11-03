let digPow n p =
    match (n.ToString().ToCharArray() 
        |> Array.mapi (fun i t -> 
            pown (int64(t.ToString())) (i+p))
        |> Array.sum) with
    | r when r % int64(n) = 0L -> r / int64(n)
    | _ -> -1L
