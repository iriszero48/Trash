open System.IO
open System
open System.Diagnostics

[<EntryPoint>]
let main argv = 
    let csp = new System.Security.Cryptography.RNGCryptoServiceProvider()
    let buf = Array.zeroCreate 8
    let mutable total = 0
    let mutable succ = 0
    let sw = new Stopwatch();
    sw.Start()
    File.ReadAllLines(argv.[0])
    |> Array.chunkBySize 8
    //|> Array.skip (Array.sum [|12;10;10;10;10; 8;8;10;10;10;|])
    //|> Array.take 10
    |> Array.sortBy (fun _ -> csp.GetBytes buf; System.BitConverter.ToUInt64(buf, 0))
    //|> Array.map (fun x -> Convert.ToInt32(x.[0]))
    |> Array.iter (fun x ->
        Console.WriteLine(String.concat " " x.[1..2]) 
        Array.iter (printfn "%s") x.[3..6]
        Console.Write("<- ")
        if Console.ReadLine().Trim().ToLower() = x.[7].Split(("你").ToCharArray()).[0].Split("：".ToCharArray(), StringSplitOptions.RemoveEmptyEntries).[1].Trim().ToLower() then
            Console.WriteLine("行")
            succ <- succ + 1
        else
            Console.BackgroundColor <- ConsoleColor.DarkRed
            Console.WriteLine("WDNMD!!!!!!!!!!!!!!!!!!")
            Console.Beep(440, 1000)
        total <- total + 1
        Console.BackgroundColor <- ConsoleColor.Black
        Console.WriteLine(x.[7].Split(("你").ToCharArray()).[0])
        Console.WriteLine()
        Console.WriteLine())
    
    sw.Stop()
    Console.WriteLine("-------------- END --------------")
    Console.WriteLine(sw.Elapsed.ToString())
    Console.WriteLine((double)succ / (double)total)
    Console.ReadLine() |> ignore
    0
