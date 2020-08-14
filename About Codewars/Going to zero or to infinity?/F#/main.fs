open System

let going (n: int) =
  let fac = Seq.scan (*) 1I [2I .. bigint(n)] in
  let d = bigint.Log(Seq.item (n - 1) fac) in
  Seq.sumBy (fun x -> exp (bigint.Log(x) - d)) fac
