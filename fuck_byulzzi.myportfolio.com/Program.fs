open HtmlAgilityPack
open System
open System.IO
open System.Net
open System.Net.Http

module fuck_byulzzi_myportfolio_com =
    let base_url = Uri("http://byulzzi.myportfolio.com/")
    let fuck_urls = {|
        work = "http://byulzzi.myportfolio.com/work";
        original = "http://byulzzi.myportfolio.com/original"
    |}
    let download_dir =
        (fun _ ->
            let dir = "byulzzi.myportfolio.com"
            if Directory.Exists dir |> not then Directory.CreateDirectory dir |> ignore
            dir)()

    let prefix = "ZZI.NET - "
    let wc = HtmlWeb()
    let download_wc =
        (fun _ ->
            let proxy = WebProxy("http://127.0.0.1:7890/")

            let header = new HttpClientHandler()
            header.Proxy <- proxy

            let down_wc = new HttpClient(header)
            down_wc)()

    type Image = {
        Filename: string
        Url: string
    }

    let download_html (url: string) =
        printfn $"-> {url}"
        wc.Load(url, "127.0.0.1", 7890, "", "")

    let auto_rename (path: string) =
        let rec loop (p: string) (i: int) =
            let new_path = Path.Combine(Path.GetDirectoryName p, $"{Path.GetFileNameWithoutExtension p}_{i}{Path.GetExtension p}")
            if File.Exists new_path then loop p (i + 1)
            else new_path
        if File.Exists path then loop path 1 else path

    let download_image (img: Image) =
        let filename = Uri(img.Url).LocalPath |> Path.GetFileName
        let new_filename = if String.IsNullOrEmpty <| img.Filename then filename else $"{img.Filename}{Path.GetExtension filename}"
        let output_path = Path.Combine(download_dir, new_filename)
        let re_output_path = if output_path |> File.Exists then auto_rename output_path else output_path
        printfn $"    {img.Url} => {re_output_path}"
        File.WriteAllBytes(re_output_path, download_wc.GetByteArrayAsync(img.Url).Result)

    let get_attr_value (attr: string) (node: HtmlNode) = node.GetAttributeValue(attr, "")

    let get_inner_text (node: HtmlNode) = node.InnerText

    let get_filename (text: string) =
        let filted_text = if text.StartsWith(prefix) then text.Substring(prefix.Length) else text
        in Path.GetInvalidFileNameChars()
        |> Array.fold (fun (acc : string) c -> acc.Replace(c.ToString(), "_")) filted_text

    let is_null_nodes (nodes: HtmlNodeCollection) = isNull nodes

    let fuck_work _ =
            (download_html fuck_urls.work)
                .DocumentNode.SelectNodes("//section[@class='project-covers']/a")
            |> Seq.map (get_attr_value "href"
                >> fun x -> Uri(base_url, x).AbsoluteUri
                >> download_html
                >> fun x -> {|
                    Title = x.DocumentNode.SelectSingleNode("//title") |> get_inner_text |> get_filename;
                    Nodes = x.DocumentNode.SelectNodes("//div[@id='project-modules']//img")
                |})
            |> Seq.filter (fun x -> not << is_null_nodes <| x.Nodes)
            |> Seq.map(fun x ->
                x.Nodes
                |> Seq.mapi (fun idx y -> {
                    Filename = $"{x.Title}_{idx}";
                    Url = get_attr_value "data-src" y
                }))
            |> Seq.concat

    let fuck_original _ =
        (download_html fuck_urls.original)
            .DocumentNode.SelectNodes("//div[@class='grid__item-container js-grid-item-container']/img")
        |> Seq.map (fun x -> {
            Filename = (get_attr_value "alt" x);
            Url = (get_attr_value "data-src" x)
        })

    let fuck _ =
        seq {
            fuck_work ();
            fuck_original ();
        }
        |> Seq.concat
        |> Seq.iter download_image


[<EntryPoint>]
let main _ =
    fuck_byulzzi_myportfolio_com.fuck ()

    printfn "Done."
    Console.ReadLine() |> ignore
    0
