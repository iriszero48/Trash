namespace FsXamlApp  
open FsXaml  
open System.IO
open System.Net
open System.Windows
open System.Collections.Generic

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
        this.username.ItemsSource <- users
        this.username.DisplayMemberPath <- "Username"
        this.username.SelectedValuePath <- "Password"
        this.username.SelectionChanged.Add (fun _ ->
            try
                this.password.Password <- this.username.SelectedValue.ToString()
            with
            | _ -> ())
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
