seq { 7 * 8 - 5 .. 56 .. 200 }
|> Seq.filter (fun x -> x % 9 = 1)
|> Seq.iter System.Console.WriteLine
