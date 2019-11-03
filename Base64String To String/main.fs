open System
open System.Text

let RemoveCRLF (str : string) =
    str.Replace("\n", "").Replace("\r", "").Replace(" ", "")

let Base64ToString (base64 : string) =
    base64
    |> RemoveCRLF
    |> Convert.FromBase64String 
    |> ASCIIEncoding.Default.GetString 
    |> Console.WriteLine

[<EntryPoint>]
let main argv = 
    if argv.Length <> 0 then
        argv.[0]
        |> Base64ToString
    else
        "Base64String To String"
        |> Console.WriteLine
        while true do
            Console.ReadLine()
            |> Base64ToString
    0
