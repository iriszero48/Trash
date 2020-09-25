open System.Text.RegularExpressions
open System.Net

let data = 
    """
    """

[<EntryPoint>]
let main argv =
    let re = Regex("""\/\/qpic.y.qq.com\/music_cover\/.+?\/300\?n=1""")
    seq { for i in re.Matches(data) -> i}
    |> Seq.distinct 
    |> Seq.iteri (fun i x -> (new WebClient()).DownloadFile("https:"+x.Value,i.ToString() + ".jpg"))
    0
