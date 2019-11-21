open System.Windows
open System.Collections.Generic

let ConvexHull (points:List<Point>) =
    points.Sort(fun a b -> a.X.CompareTo(b.X))
    let res = List<Point>()
    let ccw (a:Point) (b:Point) (c:Point) = (b.X - a.X) * (c.Y - a.Y) > (b.Y - a.Y) * (c.X - a.X)
    for p in points do
        while res.Count >= 2 && not <| ccw res.[res.Count - 2] res.[res.Count - 1] p do
            res.RemoveAt (res.Count - 1)
        res.Add p

    let t = res.Count + 1
    for i = points.Count - 1 downto 0 do
        let p = points.[i]
        while res.Count >= t && not <| ccw res.[res.Count - 2] res.[res.Count - 1] p do
            res.RemoveAt (res.Count - 1)
        res.Add p
    res.RemoveAt (res.Count - 1)
    res
