open System
open System.Text
open System.IO
open Microsoft.VisualBasic.Devices

let (|>|>) x f = f x; x

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
        |> fun x -> x.Substring(0, x.Length - (4 - lastLen)) + "h"
        
    let rec Decode (data_:string) =
        let data = data_.Substring(0, data_.Length - 1)
        let lastLen = match data.Length % 5 with 0 -> 5 | x -> x
        let padLen = data.Length / 5 * 5 + match lastLen with 5 -> 0 | _ -> 5
        data.PadRight(padLen, '~').ToCharArray()
        |> Array.chunkBySize 5
        |> Array.map (Array.map Table.IndexOf)
        |> Array.map (Array.mapi (fun i x -> (uint32)x * pown 85u (4 - i)) >> Array.sum)
        |> Array.map (fun x -> Array.init 4 (fun i -> x >>> ((3 - i) * 8) &&& 0xffu |> byte))
        |> Array.reduce Array.append
        |> Array.take ((padLen / 5) * 4 - (5 - lastLen))

let Interactive f =
    while true do
        Console.Write("<-")
        let r:string = Console.ReadLine() |> f
        Console.Write("->")
        Console.WriteLine(r)

let RenameFiles func =
    Seq.iter (fun (x:string) ->
        Console.Write(x + "=>")
        let r = func << Path.GetFileName <| x
        Computer().FileSystem.RenameFile(x, r)
        Console.WriteLine(r))

let RenameDirectories func =
    Seq.iter (fun (x:string) ->
        Console.Write(x + "=>")
        let r = func << Path.GetFileName <| x
        Computer().FileSystem.RenameDirectory(x, r)
        Console.WriteLine(r))

let (Encode:string -> string) = Encoding.UTF8.GetBytes >> Path85.Encode
let (Decode:string -> string) = Encoding.UTF8.GetString << Path85.Decode

let EncodeFiles = Directory.GetFiles >> RenameFiles Encode
let DecodeFiles = Directory.GetFiles >> RenameFiles Decode

let EncodeDirectories = Directory.GetDirectories >> RenameDirectories Encode
let DecodeDirectories = Directory.GetDirectories >> RenameDirectories Decode

let EncodeFilesAndDirectories path = path |>|> EncodeFiles |> EncodeDirectories
let DecodeFilesAndDirectories path = path |>|> DecodeFiles |> DecodeDirectories

let rec Recursive func path =
    for dir in Directory.GetDirectories(path) do Recursive func dir
    func path

let EncodeFilesRec = Recursive EncodeFiles
let DecodeFilesRec = Recursive DecodeFiles

let EncodeDirectoriesRec = Recursive EncodeDirectories
let DecodeDirectoriesRec = Recursive DecodeDirectories

let EncodeFilesAndDirectoriesRec = Recursive EncodeFilesAndDirectories
let DecodeFilesAndDirectoriesRec = Recursive DecodeFilesAndDirectories

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 when argv.[0] = "es" -> Interactive Encode
    | 1 when argv.[0] = "ds" -> Interactive Decode
    | 2 when argv.[0] = "ef" -> argv.[1] |> EncodeFiles
    | 2 when argv.[0] = "df" -> argv.[1] |> DecodeFiles
    | 2 when argv.[0] = "ed" -> argv.[1] |> EncodeDirectories
    | 2 when argv.[0] = "dd" -> argv.[1] |> DecodeDirectories
    | 2 when argv.[0] = "efd" -> argv.[1] |> EncodeFilesAndDirectories
    | 2 when argv.[0] = "dfd" -> argv.[1] |> DecodeFilesAndDirectories
    | 2 when argv.[0] = "efr" -> argv.[1] |> EncodeFilesRec
    | 2 when argv.[0] = "dfr" -> argv.[1] |> DecodeFilesRec
    | 2 when argv.[0] = "edr" -> argv.[1] |> EncodeDirectoriesRec
    | 2 when argv.[0] = "ddr" -> argv.[1] |> DecodeDirectoriesRec
    | 2 when argv.[0] = "efdr" -> argv.[1] |> EncodeFilesAndDirectoriesRec
    | 2 when argv.[0] = "dfdr" -> argv.[1] |> DecodeFilesAndDirectoriesRec
    | _ ->
        eprintfn "[ef|df|ed|dd|efd|dfd|efr|dfr|edr|ddr|efdr|dfdr|es|ds] [path]"
        eprintfn "    e/d      -> encode/decode"
        eprintfn "    f/d/fd/s -> file/directory/file&directory/string"
        eprintfn "    r        -> recursive"
    0
