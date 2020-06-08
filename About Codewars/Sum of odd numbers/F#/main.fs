let rowSumOddNumbers n = Seq.initInfinite (fun x -> 2 * x + 1) |> Seq.skip (Seq.sum [1 .. n - 1]) |> Seq.take n |> Seq.sum
