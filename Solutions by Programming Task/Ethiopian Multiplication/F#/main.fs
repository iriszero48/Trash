let EthiopianMultiplication x y =
    let rec em x y r =
        match x with
        | 1 -> y + r
        | x when x % 2 = 0 -> em (x / 2) (y * 2) r
        | _ -> em ((x - 1) / 2) (y * 2) (r + y)
    em x y 0
