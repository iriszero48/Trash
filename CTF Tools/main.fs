open System
open System.Text
open System.IO

let output (output : string) =
    printf "[!] "
    let sw = new StreamWriter("result.txt")
    sw.WriteLine(output)
    sw.Close()
    output

let getFile (path : string) =
    let sr = new StreamReader(path) 
    let cont = sr.ReadToEnd()
    sr.Close()
    cont

let removeCRLF (str : string) =
    str.Replace("\n", "").Replace("\r", "").Replace(" ", "")

let InputAndOutput (func : string -> string) tip =
    printfn "[*] 1.string"
    printfn "[*] 2.file path as string"
    printf ">"
    let result = ""
    match Console.ReadLine() with
        | "1" -> tip 
                |> printf 
                |> Console.ReadLine
                |> func 
                |> Console.WriteLine
        | "2" -> tip 
                |> printf 
                |> Console.ReadLine 
                |> getFile
                |> func 
                |> Console.WriteLine
        | _ -> printf ""

let Base64ToString (base64 : string) =
    base64
    |> removeCRLF
    |> Convert.FromBase64String 
    |> ASCIIEncoding.Default.GetString 
    |> output

let StringToBase64 str =
    Encoding.Default.GetBytes(s = str) 
    |> Convert.ToBase64String 
    |> output

[<EntryPoint>]
let main argv= 
    printfn "CTF Tools"
    let mutable cont = true
    while cont do 
        printfn "[-] other"
        printfn "[*] 00.Remove CRLF"
        printfn "[-] base64"
        printfn "[*] 11.base64 to string"
        printfn "[*] 12.string to base64"
        printfn "[*] 13.file to base64"
        printfn "[*] 14.base64 to file"
        printf ">"
        match Console.ReadLine() with
            | "00" -> InputAndOutput removeCRLF ">"
            | "11" -> InputAndOutput Base64ToString ">"
            | "12" -> InputAndOutput StringToBase64 ">"
            | ".." -> cont <- false
            | "_" -> "result.txt"
                    |> getFile
                    |> Console.WriteLine
            | _ -> Console.WriteLine("")
        printfn ""
    0
