open System

let rec Input _ =
    match Console.ReadLine() with
    | "" -> ""
    | s -> s + Input()

let next (l, h::t) = h::l, t
let previous (h::t, l) = t, h::l

let check = function
    | [], [] -> [0], [0]
    | [], r -> [0], r
    | l, [] -> l, [0]
    | a ->a

let rec skipPrevious p =
    match p with
    | ('['::_, _) -> previous p 
    | _ -> skipPrevious << previous <| p

let rec skipNext p =
    match p with
    | (_, '['::_) | (_, ']'::_) -> p
    | _ -> skipNext << next <| p

let rec Interpreter p d =
    match p,d with
    | (_, []), _ -> ()
    | (_, '>'::_), _ -> Interpreter (next p) (check << next <| d)
    | (_, '<'::_), _ -> Interpreter (next p) (check << previous <| d)
    | (_, '+'::_), (l, 255::t) -> Interpreter (next p) (l, 0::t)
    | (_, '+'::_), (l, h::t) -> Interpreter (next p) (l, (h + 1)::t)
    | (_, '-'::_), (l, 0::t) -> Interpreter (next p) (l, 255::t)
    | (_, '-'::_), (l, h::t) -> Interpreter (next p) (l, (h - 1)::t)
    | (_, ','::_), (l, _::t) -> Interpreter (next p) (l, (Console.ReadLine() |> Int32.Parse)::t)
    | (_, '.'::_), (_, h::_) -> (char)h |> Console.Write |> (fun _ -> Interpreter (next p) d)
    | (_, '['::_), (_, 0::_) -> Interpreter (skipNext << next <| p) d
    | (_, '['::_), _ | (_, ']'::_), (_, 0::_) -> Interpreter (next p) d
    | (_, ']'::_), _ -> Interpreter (skipPrevious << previous <| p) d

[<EntryPoint>]
let main _ = 
    let rec loop i =
        Console.ForegroundColor <- ConsoleColor.Yellow
        Console.Write ("bf(" + i.ToString() + ")->")
        Console.ForegroundColor <- ConsoleColor.White
        Interpreter ([], List.ofArray <| Input().ToCharArray()) ([0], [0])
        loop (i + 1)
    loop 0
    0
