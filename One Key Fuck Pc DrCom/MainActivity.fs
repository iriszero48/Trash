namespace OneKeyFuckPcDrCom

open System
open Android.App
open Android.Widget
open System.Threading
open System.Net
open System.IO

type Resources = OneKeyFuckPcDrCom.Resource

[<Activity (Label = "OneKeyFuckPcDrCom", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Activity ()
    override this.OnCreate (bundle) =
        base.OnCreate (bundle)
        this.SetContentView (Resources.Layout.Main)
        let button = this.FindViewById<Button>(Resources.Id.myButton)
        button.Click.Add (fun _ -> 
            (new Thread(new ThreadStart(fun _ ->
                let request = WebRequest.Create ("http://192.168.240.3/drcom/login?callback=dr1555552182253&DDDDD=test&upass=123456&0MKKey=123456&R1=0&R3=0&R6=0&para=00&v6ip=&_=1555484172045") :?> HttpWebRequest
                let response = request.GetResponse() :?> HttpWebResponse
                (new StreamReader(response.GetResponseStream())).ReadToEnd() |> ignore))).Start()
            button.Text <- "Fucked it!")
