> let lst = [1 .. 5];;
val lst : int list = [1; 2; 3; 4; 5]

> List.reduce (+) lst;;
val it : int = 15

> List.reduceBack (+) lst;;
val it : int = 15

> List.fold (+) 0 lst;;
val it : int = 15

> List.foldBack (+) lst 0;;
val it : int = 15

> Seq.unfold (fun (a, b) -> Some(a,(b,b+1))) (1,2);;
val it : seq<int> = seq [1; 2; 3; 4; ...]
