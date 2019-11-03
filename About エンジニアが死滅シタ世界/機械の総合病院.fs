open System
open System.Text.RegularExpressions
[<EntryPoint>]
let main _ = 
    match Console.ReadLine() with
    | s when 
        s.Length >= 6 && 
        Regex.IsMatch(s,"[A-Za-z]") &&
        Regex.IsMatch(s,"[0-9]") && 
        not <| Regex.IsMatch(s,"(.)\\1{2,}") -> "Valid"
    | _ -> "Invalid"
    |> Console.WriteLine
    0
