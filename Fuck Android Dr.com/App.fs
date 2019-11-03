namespace FsXamlApp

open System  
open FsXaml  

type App = XAML<"App.xaml">  

module Main =
    [<EntryPoint; STAThread>]  
    let main _ =
        let app = App()
        let mainWindow = new MainWindow()
        app.Run(mainWindow)
