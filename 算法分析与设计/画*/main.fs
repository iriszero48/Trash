open System
let n = Console.ReadLine() |> Convert.ToInt32
let nm = n / 2 + 1
[1..n] 
|> List.iter (fun x -> 
    Console.WriteLine(
        "".PadLeft(Math.Abs(nm - x))
        + "".PadLeft(2*(nm - Math.Abs(nm - x))-1, '*')))
