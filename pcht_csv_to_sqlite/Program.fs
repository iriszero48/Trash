open System
open Microsoft.Data.Sqlite
open Microsoft.Data.Analysis

let csv_path = @"\\TreeDiagram\K\Project\库\pcht-v1\pcht-v1.csv"
let sql_path = @""

let run _ =
    use conn = new SqliteConnection("Data Source=" + sql_path)
    conn.Open()

    use trans = conn.BeginTransaction()

    let cmd = conn.CreateCommand()
    cmd.CommandText <- "INSERT INTO PCHT (ID, PASSWORD, MD5, SHA1, NTLM, MYSQL, SHA256) VALUES ($id, $password, $md5, $sha1, $ntlm, $mysql, $sha256)"

    let id = cmd.CreateParameter()
    id.ParameterName <- "$id"

    let password = cmd.CreateParameter()
    password.ParameterName <- "$password"

    let md5 = cmd.CreateParameter()
    md5.ParameterName <- "$md5"

    let sha1 = cmd.CreateParameter()
    sha1.ParameterName <- "$sha1"

    let ntlm = cmd.CreateParameter()
    ntlm.ParameterName <- "$ntlm"

    let mysql = cmd.CreateParameter()
    mysql.ParameterName <- "$mysql"

    let sha256 = cmd.CreateParameter()
    sha256.ParameterName <- "$sha256"

    cmd.Parameters.Add(id) |> ignore
    cmd.Parameters.Add(password) |> ignore
    cmd.Parameters.Add(md5) |> ignore
    cmd.Parameters.Add(sha1) |> ignore
    cmd.Parameters.Add(ntlm) |> ignore
    cmd.Parameters.Add(mysql) |> ignore
    cmd.Parameters.Add(sha256) |> ignore

    let df = DataFrame.LoadCsv(csv_path, ',', true)
    seq { for i in 1 .. df.Rows.Count - 1 -> df.Rows.[i] }
    |> Seq.iteri (fun idx row ->
        id.Value <- row.[0]
        password.Value <- row.[1]
        md5.Value <- row.[2]
        sha1.Value <- row.[3]
        ntlm.Value <- row.[4]
        mysql.Value <- row.[5]
        sha256.Value <- row.[6]
        cmd.ExecuteNonQuery() |> ignore
        if idx % 100000 = 0 then trans.Commit())

    trans.Commit()

[<EntryPoint>]
let main _ =
    run()
    printfn "Done."
    Console.ReadLine() |> ignore
    0
