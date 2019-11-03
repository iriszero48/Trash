open System
open System.IO
open Microsoft.VisualBasic.Devices
[<EntryPoint>] 
let main _ =  
    [
        for file in "Z:\\" 
            |> Directory.GetFiles 
            |> Array.map (fun s -> 
                '\\' 
                |> s.Split  
                |> List.ofArray 
                |> List.rev 
                |> List.head) -> 
                    file
    ] 
    |> List.map (fun file ->
        match file.StartsWith("yande.re") with 
        | true -> "[*] " + file
                |> Console.WriteLine
                |> (fun a -> 
                    Computer().FileSystem.RenameFile(
                        "Z:\\" + file, 
                        file.Split('.').[1].Split(' ').[1]+"."+file.Split('.').[2]))
        | false when file.StartsWith("Konachan.com") -> "[*] " + file
                                                        |> Console.WriteLine
                                                        |> (fun e ->
                                                            Computer().FileSystem.RenameFile(
                                                                "Z:\\" + file, 
                                                                file.Split('-').[1].Split(' ').[1]+"."+file.Split('.').[2]))
        | false when file.StartsWith("pid-") -> "[*] " + file
                                                |> Console.WriteLine
                                                |> (fun _ ->
                                                    Computer().FileSystem.RenameFile(
                                                        "Z:\\" + file,
                                                        file.Split('-').[1]))
        | _ -> "[*] Match Fail"
                |> Console.WriteLine)
    |> (fun x -> 
        printfn "Done."
        |> (fun y -> 
            Console.Read()
            |> ignore))
    0
