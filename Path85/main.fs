open System.Text
open System

module Path85 =
    let ToString x = x.ToString()

    let Bind2nd f y x = f x y

    let Table = "!#$%&'()+,-.0123456789;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{}~"

    let Encode (data:byte[]) =
        let block = Array.chunkBySize 4 data
        let lastLen = block |> Array.rev |> Array.head |> Array.length
        block
        |> Array.map (fun x ->
            (match x.Length with 4 -> x | _ -> Array.append x (Array.create (4 - x.Length) 0uy))
            |> Array.mapi (fun i x -> (uint32)x <<< (3 - i) * 8) |> Array.reduce (|||))
        |> Array.map (fun (x:uint32) -> List.init 5 (fun i -> x / (pown 85u (4 - i)) % 85u |> int32))
        |> Array.reduce (@)
        |> Seq.map (Bind2nd Seq.item Table >> ToString)
        |> String.concat ""
        |> fun x -> x.Substring(0, x.Length - (4 - lastLen))
        
    let Decode (data:string) =
        let lastLen = match data.Length % 5 with 0 -> 5 | x -> x
        let padLen = data.Length / 5 * 5 + match lastLen with 5 -> 0 | _ -> 5
        data.PadRight(padLen, '~').ToCharArray()
        |> Array.chunkBySize 5
        |> Array.map (Array.map Table.IndexOf)
        |> Array.map (Array.mapi (fun i x -> (uint32)x * pown 85u (4 - i)) >> Array.sum)
        |> Array.map (fun x -> Array.init 4 (fun i -> x >>> ((3 - i) * 8) &&& 0xffu |> byte))
        |> Array.reduce Array.append
        |> Array.take ((padLen / 5) * 4 - (5 - lastLen))

    module Debug =
        let (|>|>) x f = f x; x

        let Encode (data:byte[]) =
            let block = Array.chunkBySize 4 data
            printfn "=>\n%A" block
            let lastLen = block |> Array.rev |> Array.head |> Array.length
            printfn "=>\n%A" lastLen
            block
            |> Array.map (fun x ->
                (match x.Length with 4 -> x | _ -> Array.append x (Array.create (4 - x.Length) 0uy))
                |> Array.mapi (fun i x -> (uint32)x <<< (3 - i) * 8) |> Array.reduce (|||))
            |>|> printfn "=>\n%A"
            |> Array.map (fun (x:uint32) -> List.init 5 (fun i -> x / (pown 85u (4 - i)) % 85u |> int32))
            |>|> printfn "=>\n%A"
            |> Array.reduce (@)
            |>|> printfn "=>\n%A"
            |> Seq.map (Bind2nd Seq.item Table >> ToString)
            |>|> printfn "=>\n%A"
            |> String.concat ""
            |>|> printfn "=>\n%A"
            |> fun x -> x.Substring(0, x.Length - (4 - lastLen))
            |>|> printfn "=>\n%A"

        let Decode (data:string) =
            let lastLen = match data.Length % 5 with 0 -> 5 | x -> x
            printfn "=>\n%A" lastLen
            let padLen = data.Length / 5 * 5 + match lastLen with 5 -> 0 | _ -> 5
            printfn "=>\n%A" padLen
            data.PadRight(padLen, '~').ToCharArray()
            |>|> printfn "=>\n%A"
            |> Array.chunkBySize 5
            |>|> printfn "=>\n%A"
            |> Array.map (Array.map Table.IndexOf)
            |>|> printfn "=>\n%A"
            |> Array.map (Array.mapi (fun i x -> (uint32)x * pown 85u (4 - i)) >> Array.sum)
            |>|> printfn "=>\n%A"
            |> Array.map (fun x -> Array.init 4 (fun i -> x >>> ((3 - i) * 8) &&& 0xffu |> byte))
            |>|> printfn "=>\n%A"
            |> Array.reduce Array.append
            |>|> printfn "=>\n%A"
            |> Array.take ((padLen / 5) * 4 - (5 - lastLen))
            |>|> printfn "=>\n%A"
        
[<EntryPoint>]
let main _ = 
    let (EncodeString:string -> string) = Encoding.UTF8.GetBytes >> Path85.Encode
    let (DecodeString:string -> string) = Encoding.UTF8.GetString << Path85.Decode

    let str = "阿姆斯特朗回旋加速喷气式超电磁炮！"
    let encoded = EncodeString str
    let decoded = DecodeString encoded

    printfn "%A => %A" str encoded
    printfn "%A => %A" encoded decoded

    printfn "----------DEBUG----------"

    let (EncodeStringDebug:string -> string) = Encoding.UTF8.GetBytes >> Path85.Debug.Encode
    let (DecodeStringDebug:string -> string) = Encoding.UTF8.GetString << Path85.Debug.Decode

    let encodedDebug = EncodeStringDebug str
    printfn "%A => %A" str encoded
    let decodedDebug = DecodeStringDebug encoded
    printfn "%A => %A" encoded decoded

    Console.ReadLine() |> ignore
    0
