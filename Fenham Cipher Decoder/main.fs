open System
open System.Text
open System.Text.RegularExpressions

let FenhamCipherDecode (input : string) (key : string) =
    [|
        for i in Array.zip 
            (
                input 
                |> Regex(".{7}").Matches 
                |> Seq.cast<Match>  
                |> Seq.map (fun m -> m.Value)  
                |> Seq.toArray
                |> Array.map (fun x -> Convert.ToByte(x,2))
            ) 
            (
                key.ToCharArray() 
                |> ASCIIEncoding.ASCII.GetBytes
            ) 
            do yield 
                (i|>fst)^^^(i|>snd)
    |] 
    |> Encoding.ASCII.GetString
    |> Console.WriteLine

[<EntryPoint>]
let main argv=
    if argv.Length <> 0 then
        FenhamCipherDecode argv.[0] argv.[1]
    else
        "Fenham Cipher Decoder"
        |> Console.WriteLine
        while true do
            FenhamCipherDecode (Console.ReadLine()) (Console.ReadLine())
    0
