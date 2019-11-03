open System
open System.IO

let mutable input = 
    String.Empty
let mutable result = 
    List.empty

let Update ch = 
    let files = 
        "Z:\\" 
        |> Directory.GetFiles
    input <- 
        input + ch
    result <- 
        [for file in files 
            |> Array.map (fun s ->
                '\\' 
                |> s.Split  
                |> List.ofArray
                |> List.rev
                |> List.head) do 
                    if file.ToLower().StartsWith (input.ToLower()) then 
                        yield file]
    result

let Output none =
    Console.Clear()
    input
    |> Console.WriteLine
    for res in result do 
        for r in Array.zip (res.ToCharArray()) ([|for i in 1 .. res.Length -> i|]) do
            match (r |> snd) <= input.Length with
                | true -> Console.BackgroundColor <- ConsoleColor.Blue
                | _ -> Console.BackgroundColor <- ConsoleColor.Black
            r |> fst |> Console.Write
        Console.BackgroundColor <- ConsoleColor.Black
        Environment.NewLine |> Console.Write

[<EntryPoint>] 
let main _ =  
    String.Empty
    |> Update
    |> Output
    while true do
        let inputKey = 
            Console.ReadKey()
        match inputKey.Key with 
            | ConsoleKey.Enter -> 
                if result.Length = 1 then 
                    File.Move("Z:\\" + result.Head, "Z:\\1\\" + result.Head)
                    |> (fun x -> 
                        input <-
                            String.Empty)
                    |> (fun x -> 
                        String.Empty) 
                    |> Update 
                    |> Output
            | ConsoleKey.Backspace -> 
                if input.Length > 0 then 
                    (input <- 
                        input.Substring(0, input.Length - 1)) 
                    |> (fun x -> 
                        String.Empty) 
                    |> Update 
                    |> Output
            | ConsoleKey.Tab ->
                (input <-
                    result.Head) 
                |> (fun x -> 
                    String.Empty) 
                |> Update 
                |> Output
            | _ -> 
                string inputKey.KeyChar 
                |> Update 
                |> Output
    0
