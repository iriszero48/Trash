open System
            
let stat(strg: string): string =
    if String.IsNullOrEmpty strg then ""
    else
      let data = strg.Split(',') |> Array.map ((fun x -> DateTime.Parse(x.Trim().Replace("|",":")).Ticks) >> double) |> Array.sort in
      let result =
        [| Array.max data - Array.min data;
        Array.average data;
        (let len = data.Length in if len &&& 1 = 0 then Array.average <| data.[len/2-1..len/2] else data.[len/2]) |]
      |> Array.map (fun x -> (new TimeSpan(int64(x))).ToString("hh\|mm\|ss")) in
      sprintf "Range: %s Average: %s Median: %s" (result.[0]) (result.[1]) (result.[2])
