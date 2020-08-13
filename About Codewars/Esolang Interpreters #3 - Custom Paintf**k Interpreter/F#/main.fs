open System

let rec skipPrevious p c i iterations =
    match p,i with
    | ']'::t, i -> skipPrevious t (']'::c) (i + 1) iterations
    | '['::t, 0 -> iterations, t, '['::c
    | '['::t, i -> skipPrevious t ('['::c) (i - 1) iterations
    | h::t, i -> skipPrevious t (h::c) i iterations

let rec skipNext p c i iterations =
    match c,i with
    | '['::t, i -> skipNext ('['::p) t (i + 1) iterations
    | ']'::t, 0 -> iterations, ']'::p, t
    | ']'::t, i -> skipNext (']'::p) t (i - 1) iterations
    | h::t, i -> skipNext (h::p) t i iterations

let interpret (code: string) (data: bool[,]) width height iterations =
    let rec fxx x y = function
        | (0, _, _) | (_, _, [])    -> data
        | (i, p, h::t) when h = 'n' -> fxx ((x + height - 1) % height) y                         (i - 1, h::p, t)
        | (i, p, h::t) when h = 'e' -> fxx x                           ((y + 1) % width)         (i - 1, h::p, t)
        | (i, p, h::t) when h = 's' -> fxx ((x + 1) % height)          y                         (i - 1, h::p, t)
        | (i, p, h::t) when h = 'w' -> fxx x                           ((y + width - 1) % width) (i - 1, h::p, t)
        | (i, p, h::t) when h = '*' ->
            data.[x,y] <- not data.[x,y]
            fxx x y (i - 1, h::p, t)
        | (i, p, h::t) when h = '[' && date.[x,y] -> fxx x y (i - 1, h::p, t)
        | (i, p, h::t) when h = '[' -> fxx x y (skipNext (h::p) t 0 (i - 1))
        | (i, p, h::t) when h = ']' -> fxx x y (skipPrevious p (h::t) 0 i)
        | (i, p, h::t) -> fxx x y (i, h::p, t)
    fxx 0 0 (iterations, [], code.ToCharArray() |> Array.toList)

let interpreter (code: string) iterations width height =
    interpret code (Array2D.zeroCreate height width) width height iterations
    |> Seq.cast<bool>
    |> Seq.chunkBySize width
    |> Seq.map (Seq.map (Convert.ToInt32 >> Convert.ToString) >> String.concat "")
    |> String.concat "\r\n"
