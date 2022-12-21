open Npgsql
open System
open FSharp.Data
open System.Collections.Generic
open System.Text

let csv_path = @"E:\tmp\pcht-v1.csv\pcht-v1.csv"

let get_rows (path: string) =
    let df = CsvFile.Load(path)
    seq { for i in df.Rows -> i }

[<EntryPoint>]
let main _ =
    printf "> "
    let conn_string = $"Server=localhost;Port=5432;User Id=postgres;Password={Console.ReadLine()};Database=pcht;"

    let conn = new NpgsqlConnection(conn_string)
    conn.Open()

    let trans = conn.BeginTransaction()
    
    let cmd = new NpgsqlCommand("INSERT INTO pcht (id, password, md5, sha1, ntlm, mysql, sha256) 
    VALUES (@id, @password, @md5, @sha1, @ntlm, @mysql, @sha256)", conn, trans)

    let id = new NpgsqlParameter("id", NpgsqlTypes.NpgsqlDbType.Bigint)
    let password = new NpgsqlParameter("password", NpgsqlTypes.NpgsqlDbType.Bytea)
    let md5 = new NpgsqlParameter("md5", NpgsqlTypes.NpgsqlDbType.Bytea)
    let sha1 = new NpgsqlParameter("sha1", NpgsqlTypes.NpgsqlDbType.Bytea)
    let ntlm = new NpgsqlParameter("ntlm", NpgsqlTypes.NpgsqlDbType.Bytea)
    let mysql = new NpgsqlParameter("mysql", NpgsqlTypes.NpgsqlDbType.Bytea)
    let sha256 = new NpgsqlParameter("sha256", NpgsqlTypes.NpgsqlDbType.Bytea)

    cmd.Parameters.Add(id) |> ignore
    cmd.Parameters.Add(password) |> ignore
    cmd.Parameters.Add(md5) |> ignore
    cmd.Parameters.Add(sha1) |> ignore
    cmd.Parameters.Add(ntlm) |> ignore
    cmd.Parameters.Add(mysql) |> ignore
    cmd.Parameters.Add(sha256) |> ignore

    let mutable id_sets = new HashSet<int64>()
    for i in get_rows csv_path do
        if i.Columns.Length <> 7 then failwith "Invalid number of columns"

        let id_val = Convert.ToInt64(i.[0])
        if not <| id_sets.Contains(id_val) then 
            id.Value <- id_val
            password.Value <- Encoding.UTF8.GetBytes i.[1]
            md5.Value <- Convert.FromHexString i.[2]
            sha1.Value <- Convert.FromHexString i.[3]
            ntlm.Value <- Convert.FromHexString i.[4]
            mysql.Value <- Convert.FromHexString(i.[5].TrimStart('*'))
            sha256.Value <- Convert.FromHexString i.[6]
            cmd.ExecuteNonQuery() |> ignore
            
            id_sets.Add(id_val) |> ignore

    trans.Commit()
    printfn "Inserted %d rows" id_sets.Count

    printfn "Done."
    Console.ReadLine() |> ignore
    0


