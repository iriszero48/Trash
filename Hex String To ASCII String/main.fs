open System

let HexStringToASCIIString (hexString : string) = 
    [
        for i in 0 .. 2 .. hexString.Length-2 do 
            yield Uri.HexUnescape
                (   
                    (
                        [|
                            "%";
                            string hexString.[i];
                            string hexString.[i+1]
                        |] 
                        |> Array.fold (+) ""
                    ),
                    ref 0
                ) 
                |> string
    ] 
    |> List.fold (+) "" 
    |> Console.WriteLine
    
[<EntryPoint>]
let main argv = 
    if argv.Length <> 0 then
        argv.[0]
        |> HexStringToASCIIString
    else
        "Hex String To ASCII String"
        |> Console.WriteLine
        while true do
            Console.ReadLine()
            |> HexStringToASCIIString
    0
