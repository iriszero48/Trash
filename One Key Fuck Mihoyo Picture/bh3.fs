open System.Text.RegularExpressions
open System.Net
open System.IO

let data = // data.txt

[<EntryPoint>]
let main _ = 
    seq{for i in Regex.Matches(data,"""https:\/\/uploadstatic\.mihoyo\.com\/contentweb\/\d{8}\/\d{19}\.(png|jpg)""") -> i.Value}
    |> Seq.distinct
    |> Seq.iteri (fun i x -> (new WebClient()).DownloadFile(x, i.ToString() + Path.GetExtension(x)) |> printfn "%A") 
    0
