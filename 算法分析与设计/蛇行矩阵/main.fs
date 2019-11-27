open System

type Op = UpRight | DownLeft

let ZigZagMatrix m n =
    let map = Array2D.create m n 1
    let rec run x y i op =
        map.[x, y] <- i
        match op with
        | _        when x = m - 1 && y = n - 1 -> map
        | DownLeft when x = m - 1              -> run x       (y + 1) (i + 1) UpRight
        | UpRight  when y = n - 1              -> run (x + 1) y       (i + 1) DownLeft
        | DownLeft when y = 0                  -> run (x + 1) 0       (i + 1) UpRight
        | UpRight  when x = 0                  -> run 0       (y + 1) (i + 1) DownLeft
        | DownLeft                             -> run (x + 1) (y - 1) (i + 1) DownLeft
        | _                                    -> run (x - 1) (y + 1) (i + 1) UpRight 
    run 0 0 1 UpRight

let SnakeMatrix x = ZigZagMatrix x x

[<EntryPoint>]
let main _ = 
    while true do
        match Console.ReadLine() with
        | null -> exit 0
        | str ->
            let n = Int32.Parse str
            SnakeMatrix n
            |> Seq.cast<int>
            |> Seq.chunkBySize n
            |> Seq.map (Array.map Convert.ToString >> String.concat " ")
            |> Seq.iter (printfn "%s")
    0
