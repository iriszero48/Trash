let squareDigitsSequence a0 =
    let rec data = seq {
        yield a0;
        for n in data ->
            n.ToString().ToCharArray()
            |> Array.map (int >> ((+) -48) >> (fun x -> x * x))
            |> Array.sum } in
    let rec check lst i =
        let n = Seq.item i data in
        if List.contains n lst then i + 1 else check (n::lst) (i + 1) in
    check [] 0
