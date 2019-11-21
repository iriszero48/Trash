let BubbleSort lst =
    let rec bs acc tail succ =
        match tail, succ with
        | [], true -> acc
        | [], _ -> bs [] acc true
        | x::y::t, _ when x > y -> bs (acc@[y]) (x::t) false
        | h::t, _ -> bs (acc@[h]) t succ
    bs [] lst true
