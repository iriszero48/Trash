open System

let Judge X Y x y (a2d : int[,]) =
    let mutable res = true
    try
        for i in [X .. x] do
            for j in [Y .. y] do
                if a2d.[i,j] <> 0 then
                    res <- false
    with
    | _ -> res <- false
    res

let Draw X Y x y id (a2d : int[,]) =
    for i in [X .. x] do
        for j in [Y .. y] do
            a2d.[i,j] <- id

[<EntryPoint>]
let main _ =
    let H::W::N::_ = Console.ReadLine().Trim().Split(' ') |> Array.map Convert.ToInt32 |> List.ofArray
    let mutable home = Array2D.init H W (fun _ _ -> 0)
    let input = [for i in [1 .. N] -> i::(Console.ReadLine().Trim().Split(' ') |> Array.map Convert.ToInt32 |> List.ofArray)]
    let id::h::w::x::y::_ = input |> List.maxBy (fun x -> x.[0] * x.[1])
    if x = 1 then home <- Array2D.mapi (fun x _ i -> if x = 0 then -1 else i) home
    if x = H then home <- Array2D.mapi (fun x _ i -> if x = H - 1 then -1 else i) home
    if y = 1 then home <- Array2D.mapi (fun _ y i -> if y = 0 then -1 else i) home
    if y = W then home <- Array2D.mapi (fun _ y i -> if y = W - 1 then -1 else i) home
    for i in [0 .. Array2D.length1 home - 1] do
        for j in [0 .. Array2D.length2 home - 1] do
            if Judge i j (h + i - 1) (w + j - 1) home then
                Draw i j (h + i - 1) (w + j - 1) id home
                home <- Array2D.map (fun i -> if i = -1 then 0 else i) home
                for line in home |> Seq.cast<int> |> Seq.chunkBySize W do
                    line |> Array.map Convert.ToString |> Array.reduce (fun a b -> a + " " + b) |> Console.WriteLine
                exit 0
    0
