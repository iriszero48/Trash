open System

let listSquared m n =
    List.filter (fun (_, s) -> let r = int(round(sqrt(float s))) in r * r = s)
        [for x in m..n -> x, List.sum [for y in 1..x do if x % y = 0 then yield y * y]]
