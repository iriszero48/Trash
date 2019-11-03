namespace fuckpcdrcom

open System
open Android.App
open Android.Widget
open System.Net
open System.IO
open System.Threading

type Resources = App3.Resource

[<Activity (Label = "Fuckpcdrcom", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Activity ()
    override this.OnCreate (bundle) =
        base.OnCreate (bundle)
        this.SetContentView (Resources.Layout.Main)
        let cb = this.FindViewById<EditText>(Resources.Id.cb)
        let ur = this.FindViewById<EditText>(Resources.Id.ur)
        let pw = this.FindViewById<EditText>(Resources.Id.pw)
        let button = this.FindViewById<Button>(Resources.Id.fuck)
        button.Click.Add (fun _ ->
            try
                (new Thread(new ThreadStart(fun _ ->
                    let request = WebRequest.Create ("http://192.168.240.3/drcom/login?callback=" + cb.Text + "&DDDDD=" + ur.Text + "&upass=" + pw.Text + "&0MKKey=123456&R1=0&R3=0&R6=0&para=00&v6ip=&_=1555484172045") :?> HttpWebRequest
                    let response = request.GetResponse() :?> HttpWebResponse
                    (new StreamReader(response.GetResponseStream())).ReadToEnd() |> ignore))).Start()
            with
            | _ -> ())
