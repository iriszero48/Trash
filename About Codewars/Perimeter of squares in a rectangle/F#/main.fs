open System.Numerics  
let perimeter (n: BigInteger): BigInteger =
  Seq.unfold (fun (x, y) -> Some(x, (y, x + y))) (0I,1I)
  |> Seq.skip 1
  |> Seq.take (int32(n + 1I))
  |> Seq.sum
  |> (*) 4I
