open System.Net
open System.IO
open System

[<EntryPoint>]
let main _ = 
    let request = WebRequest.Create ("http://192.168.240.3/drcom/login?callback=dr1556446265749&DDDDD=test&upass=123456&0MKKey=123456&R1=0&R3=0&R6=1&para=00&v6ip=&_=1556445263713") :?> HttpWebRequest
    let response = request.GetResponse() :?> HttpWebResponse
    (new StreamReader(response.GetResponseStream())).ReadToEnd() |> ignore
    printf "{ok}."
    Console.ReadLine() |> ignore
    0
