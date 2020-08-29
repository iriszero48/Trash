open System.Text.RegularExpressions
open System.Net
open System.IO

let data = """""" // bh2.txt

[<EntryPoint>]
let main _ = 
    seq{for i in Regex.Matches(data,"""\/\/static-event\.benghuai\.com\/new_mihoyo_homepage\/images\/download\/cg\/origin\/20\d\d-\d\d-\d\d.*?\.(png|jpg)""") -> "https:" + i.Value}
    |> Seq.distinct
    |> Seq.iteri (fun i x -> (new WebClient()).DownloadFile(x, i.ToString() + Path.GetExtension(x)) |> printfn "%A") 
    0
