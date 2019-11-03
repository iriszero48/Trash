open System
[<EntryPoint>]
let main _ = 
    let line = Console.ReadLine() |> Convert.ToInt32
    let rec input i =
        match i with
        i when i > 0 -> 
            match Console.ReadLine().Split(' ') |> List.ofArray with
            | s::e::_ when e = "3" -> s::input (i - 1)
            | _ -> input (i - 1)
        | _ -> []
    input line |> List.iter Console.WriteLine
    0
