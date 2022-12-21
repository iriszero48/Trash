open System
open Microsoft.Data.Sqlite
open Microsoft.Data.Analysis
open System.Collections.Generic
open FSharp.Data

type ColValue = 
    | Integer of int64
    | Text of string


let csv_to_sqlite (table: string) (csv_path: string) (sqlite_path: string) (has_head: bool) (proc_handle: string[] -> ColValue[]) =
    let conn = new SqliteConnection(sprintf "Data Source=%s" sqlite_path)
    conn.Open()

    use trans = conn.BeginTransaction()
    trans.Save("init")

    //let df = DataFrame.LoadCsv(filename=csv_path, separator=',', header=has_head, encoding=Text.Encoding.UTF8)
    let df = CsvFile.Load(csv_path)
    let cols_count = df.NumberOfColumns
    // let cols_count = df.Columns.Count

    let query_params_name = Array.init cols_count (sprintf "$param%d")
    printfn "%A" query_params_name

    let cmd = conn.CreateCommand()
    cmd.CommandText <- sprintf "INSERT INTO %s VALUES (%s)" table (String.concat "," query_params_name)

    let query_params = Array.init cols_count (fun i ->
        let p = cmd.CreateParameter()
        p.ParameterName <- query_params_name.[i]
        p)

    printfn "%s" cmd.CommandText

    for i in 0..query_params_name.Length - 1 do
        query_params.[i].ParameterName <- query_params_name.[i]
        cmd.Parameters.Add(query_params.[i]) |> ignore

    let mutable count = 0
    for row in  seq { for i in df.Rows -> Array.init cols_count (fun x -> i.[x] ) } do
        let data = proc_handle row

        for j in 0 .. data.Length - 1 do 
            match data.[j] with
            | Integer v ->
                query_params.[j].Value <- v
                query_params.[j].SqliteType <- SqliteType.Integer
            | Text v ->
                query_params.[j].Value <- v
                query_params.[j].SqliteType <- SqliteType.Text
                
        cmd.ExecuteNonQuery() |> ignore
        count <- count + 1

        if count % 100000 = 0 then printfn "Processed %d" count

    trans.Commit()
    printfn "Processed %d" count

[<EntryPoint>]
let main _ =
    let table = "fmd"
    let csv_path = @"E:\FMD.210117\fmd_std.csv"
    let sqlite_path = @"E:\FMD.210117\fmd.210117.sqlite"
    let has_head = true
    let proc_handle = (fun (x: string[]) ->
        let dev = x.[0]
        let dir = x.[1]
        let file = x.[2]
        let md5 = x.[3]
        let size = x.[4]
        let time = x.[5]

        [| Text dev
           Text dir
           Text file
           Text md5
           Integer (Int64.Parse size)
           Text time |])

    csv_to_sqlite table csv_path sqlite_path has_head proc_handle

    printfn "Done."
    Console.ReadLine() |> ignore
    0 // return an integer exit code