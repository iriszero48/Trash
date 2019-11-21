open System.Windows

let ClosestPairs (points:Point []) =
    let n = points.Length - 1
    in seq {
        for i in 0..n-1 do
            for j in i+1..n do
                yield points.[i], points.[j]
    }
    |> Seq.minBy (fun (a, b) -> (b - a).LengthSquared)
