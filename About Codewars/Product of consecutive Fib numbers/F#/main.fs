let productFib (n: uint64) =
    let fib x_ =
        let x = float(x_) in
        uint64(2. ** -x * ((1. + sqrt 5.) ** x - (-1. + sqrt 5.) ** x * cos(System.Math.PI * x)) / sqrt 5.) in
    let l, r, v =
        Seq.initInfinite (fun x ->
            let f0 = fib x in
            let f1 = fib (x + 1) in
            f0, f1, f0 * f1)
        |> Seq.find (fun (_, _, v) -> v >= n) in
    l, r, v = n
