module FSharpUtils

open System
open System.IO
open System.Net.Http
open System.Text.Json
open System.Collections.Generic

module FuNet =
    let DownloadFile url path =
        task {
            use client = new HttpClient()
            use file = File.OpenWrite(path)
            let! resp = client.GetAsync(Uri(url))
            do! resp.Content.CopyToAsync(file)
        }
        |> Async.AwaitTask
        |> Async.RunSynchronously

module FuJson =
    let Load (json: string) =
        JsonSerializer.Deserialize<JsonElement>(json, JsonSerializerOptions())

    let As<'a> (json: JsonElement) =
        JsonSerializer.Deserialize<'a>(json, JsonSerializerOptions())

    let AsObject = As<Dictionary<string, JsonElement>>
    let AsArray = As<List<JsonElement>>
    let AsString = As<string>

    let Get key (json: JsonElement) = (AsObject json).[key]
    let Item index (json: JsonElement) = (AsArray json).[index]

    let Dump<'a> (data: 'a) =
        JsonSerializer.Serialize(data, JsonSerializerOptions())

    let Pretty<'a> (data: 'a) =
        let opt = JsonSerializerOptions()
        opt.WriteIndented <- true
        JsonSerializer.Serialize(data, opt)

module FuFileSystem =
    let FileSize path = (new FileInfo(path)).Length
