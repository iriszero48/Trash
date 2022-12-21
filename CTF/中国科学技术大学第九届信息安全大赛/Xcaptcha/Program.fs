open System.Net.Http
open System
open System.Numerics
open System.Collections.Generic

let fuck_url = "http://202.38.93.111:10047/?token=1752%3AMEYCIQDj3nBpz9cXMRR%2BregAdsX9KIr0%2B3LU1wLseGSC1AJbTwIhALMTGTOEXgW2VcmvyNPf%2FMxsaCvSp9YtPCoRFBpuZomx"
let fuck_url1 = "http://202.38.93.111:10047/xcaptcha"

let client = new HttpClient()

let (|>|>) f x = f (x); x

let get_result tsk = tsk |> Async.AwaitTask |> Async.RunSynchronously

let get_cookie _ =
    let req = new HttpRequestMessage(HttpMethod.Get, fuck_url)
    let resp = client.Send(req)
    resp.Content.ReadAsStringAsync()
    |> get_result
    |> printfn "%s"

get_cookie()

let req = new HttpRequestMessage(HttpMethod.Get, fuck_url1)
let resp = client.Send(req)
let data = 
    resp.Content.ReadAsStringAsync()
    |> get_result
    |> fun x -> x.Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.StartsWith("<label for=\"captcha"))
    |> (fun x -> printfn "%A" x; x)
    |> Seq.map (fun x -> x.Split(">").[1].Split("的结果是？").[0].Trim())
    |> Seq.map (fun x ->
        x.Split("+")
        |> (fun x -> printfn "%A" x; x)
        |> Seq.map BigInteger.Parse
        |> Seq.reduce (+))
    |> Seq.map (fun x -> x.ToString())
    |> (fun x -> printfn "%A" x; x)
    |> Seq.mapi (fun i x -> new KeyValuePair<string, string>(sprintf "captcha%d" (i + 1), x))

let ctx = new FormUrlEncodedContent(data)
let res = client.PostAsync(fuck_url1, ctx) |> Async.AwaitTask |> Async.RunSynchronously
res.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously
|> printfn "%A"
    


printfn "Done."
Console.ReadLine() |> ignore
