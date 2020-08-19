let solvePuzzle (clues : int[]) : int[][] =
    let con = [|
        [|1; 2; 3; 4|];
        [|1; 2; 4; 3|];[|1; 3; 2; 4|];[|1; 3; 4; 2|];[|2; 1; 3; 4|];[|2; 3; 1; 4|];[|2; 3; 4; 1|];
        [|1; 4; 2; 3|];[|1; 4; 3; 2|];[|2; 1; 4; 3|];[|2; 4; 1; 3|];[|2; 4; 3; 1|];[|3; 1; 2; 4|];[|3; 1; 4; 2|];[|3; 2; 1; 4|];[|3; 2; 4; 1|];[|3; 4; 1; 2|];[|3; 4; 2; 1|];
        [|4; 1; 2; 3|];[|4; 1; 3; 2|];[|4; 2; 1; 3|];[|4; 2; 3; 1|];[|4; 3; 1; 2|];[|4; 3; 2; 1|]
    |]
    seq {
        for a0 in con do
            for a1 in con do
                for a2 in con do
                    for a3 in con do
                        let board = Array.concat [a0;a1;a2;a3]
                        let GetXs x = [|for i in 0..4..15 -> board.[i + x]|]
                        let GetYs y = board.[y*4..y*4+3]
                        let vi (lst:int[]) v = Seq.mapi (fun i v -> Seq.forall (fun li -> v >= li) (lst.[0..i])) lst |> Seq.filter (fun x -> x) |> Seq.length = v
                        if  Seq.forall (GetXs >> Seq.distinct >> Seq.length >> (=) 4) [0..3] &&
                            Seq.forall (GetYs >> Seq.distinct >> Seq.length >> (=) 4) [0..3] &&
                            Seq.zip [0..3]     clues.[0..3]   |> Seq.filter (snd >> (<>) 0) |> Seq.forall (fun (x,v) -> vi (GetXs x) v) &&
                            Seq.zip [0..3]     clues.[4..7]   |> Seq.filter (snd >> (<>) 0) |> Seq.forall (fun (x,v) -> vi (GetYs x |> Array.rev) v) &&
                            Seq.zip [3..-1..0] clues.[8..11]  |> Seq.filter (snd >> (<>) 0) |> Seq.forall (fun (x,v) -> vi (GetXs x |> Array.rev) v) &&
                            Seq.zip [3..-1..0] clues.[12..15] |> Seq.filter (snd >> (<>) 0) |> Seq.forall (fun (x,v) -> vi (GetYs x) v) then
                                yield board
    } |> Seq.head |> Array.chunkBySize 4
