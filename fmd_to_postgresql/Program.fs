open Npgsql
open System
open FSharp.Data

let csv_path = @"\\Treediagram\m\fmd.221005\fmd.csv"

let get_rows (path: string) =
    let df = CsvFile.Load(path)
    seq { for i in df.Rows -> i }

[<EntryPoint>]
let main _ =
    printf "> "
    let conn_string = $"Server=localhost;Port=5432;User Id=postgres;Password={Console.ReadLine()};Database=fmd;"

    let conn = new NpgsqlConnection(conn_string)
    conn.Open()

    let trans = conn.BeginTransaction()
    
    let cmd = new NpgsqlCommand("INSERT INTO fmd (file_path, file_md5, file_size, last_write_time) 
    VALUES ((@device_name, @parent_path, @filename), @file_md5, @file_size, @last_write_time)", conn, trans)

    let dev = new NpgsqlParameter("device_name", NpgsqlTypes.NpgsqlDbType.Text)
    let parent = new NpgsqlParameter("parent_path", NpgsqlTypes.NpgsqlDbType.Text)
    let filename = new NpgsqlParameter("filename", NpgsqlTypes.NpgsqlDbType.Text)
    let md5 = new NpgsqlParameter("file_md5", NpgsqlTypes.NpgsqlDbType.Bytea)
    let size = new NpgsqlParameter("file_size", NpgsqlTypes.NpgsqlDbType.Bigint)
    let last_write = new NpgsqlParameter("last_write_time", NpgsqlTypes.NpgsqlDbType.Timestamp)

    cmd.Parameters.Add(dev) |> ignore
    cmd.Parameters.Add(parent) |> ignore
    cmd.Parameters.Add(filename) |> ignore
    cmd.Parameters.Add(md5) |> ignore
    cmd.Parameters.Add(size) |> ignore
    cmd.Parameters.Add(last_write) |> ignore

    let mutable count = 0UL
    for i in get_rows csv_path do
        dev.Value <- i.[0]
        parent.Value <- i.[1]
        filename.Value <- i.[2]
        md5.Value <- Convert.FromHexString(i.[3])
        size.Value <- Convert.ToInt64(i.[4])
        last_write.Value <- System.DateTime.Parse(i.[5], Globalization.CultureInfo.InvariantCulture)

        printfn "%A" i
        cmd.ExecuteNonQuery() |> ignore
        count <- count + 1UL

    trans.Commit()
    printfn "Inserted %d rows" count

    printfn "Done."
    Console.ReadLine() |> ignore
    0


