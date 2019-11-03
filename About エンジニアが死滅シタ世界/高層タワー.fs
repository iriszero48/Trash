open System
[<EntryPoint>]
let main _ =
    let rec cal (a : string) (b : string) =
        match a with 
        | x when b.StartsWith x -> b.Substring x.Length
        | _ -> cal (a.Substring 1) b 
    [for i in 
        [1 .. Console.ReadLine().Trim() |> Convert.ToInt32] -> 
            Console.ReadLine().Trim()] 
    |> List.reduce (fun x y -> x + cal x y) 
    |> Console.WriteLine
    0
