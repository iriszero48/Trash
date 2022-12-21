open System.IO

let src_file = @"E:\PROJECT\HACKERGAME2022\src\getflag.hei.py"
let dst_file = @"getflag.py"

seq {
    for line in File.ReadAllLines(src_file) do
        if line.StartsWith("a[") then
            let token = line.Split("=")
            let value = token.[1].Trim()
            yield! token.[0].Substring(2).Split("|")
                |> Seq.map (fun idx -> idx.Replace("[", "").Replace("]", "").Trim())
                |> Seq.map (fun idx -> sprintf "a[%s] = %s" idx value)
        else
            yield line
}
|> fun x -> File.WriteAllLines(dst_file, x)
