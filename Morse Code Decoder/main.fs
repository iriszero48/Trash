open System

let morseTable() : Map<string, string> = 
    [|
        ("A",".-");
        ("B","-...");
        ("C","-.-.");
        ("D","-.."); 
        ("E","."); 
        ("F","..-.");
        ("G","--."); 
        ("H","....");
        ("I","..");  
        ("J",".---"); 
        ("K","-.-"); 
        ("L",".-..");
        ("M","--"); 
        ("N","-."); 
        ("O","---"); 
        ("P",".--.");
        ("Q","--.-"); 
        ("R",".-.");  
        ("S","..."); 
        ("T","-"); 
        ("U","..-"); 
        ("V","...-"); 
        ("W",".--");  
        ("X","-..-");
        ("Y","-.--"); 
        ("Z","--..");
        ("1",".----");
        ("2","..---");
        ("3","...--");
        ("4","....-");
        ("5",".....");
        ("6","-....");
        ("7","--...");
        ("8","---..");
        ("9","----.");
        ("0","-----");
        ("?","..--..");
        ("/","-..-.");
        ("-","-....-");
        (".",".-.-.-");
    |]
    |> Map.ofArray 
    |> (fun map ->  Map.fold (fun (mores : Map<string, string>) key value -> mores.Add(value, key)) Map.empty map)

let MorseCodeDecode (input : string) =
    [for i in input.Trim().Replace('/',' ').Split(' ') do yield morseTable().[i]]
    |> List.fold (+) ""
    |> Console.WriteLine

[<EntryPoint>]
let main argv=
    if argv.Length <> 0 then
        argv.[0]
        |> MorseCodeDecode
    else
        "Morse Code Decoder"
        |> Console.WriteLine
        while true do
            Console.ReadLine()
            |> MorseCodeDecode
    0
