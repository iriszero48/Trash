let mix (s1: string) (s2: string): string =
    let wc =
        Seq.filter (fun x -> List.contains x ['a'..'z'])
        >> Seq.countBy (fun x -> x) 
        >> Seq.filter (snd >> ((<) 1)) 
        >> Seq.sortBy (fun (w,_) -> w) 
        >> Seq.toList
    let rec fxx = function
        | [],[] -> []

        | (w1,c1)::t1,(w2,c2)::t2 when w1 = w2 && c1 = c2 -> ("=:" + String.replicate c1 (string w1))::fxx (t1, t2)
        | (w1,c1)::t1,(w2,c2)::t2 when w1 = w2 && c1 > c2 -> ("1:" + String.replicate c1 (string w1))::fxx (t1, t2)
        | (w1,c1)::t1,(w2,c2)::t2 when w1 = w2            -> ("2:" + String.replicate c2 (string w2))::fxx (t1, t2)

        | (w1,c1)::t1,(w2,c2)::t2 when w1 < w2 -> ("1:" + String.replicate c1 (string w1))::fxx (t1, (w2,c2)::t2)
        | (w1,c1)::t1,(w2,c2)::t2              -> ("2:" + String.replicate c2 (string w2))::fxx ((w1,c1)::t1, t2)

        | (w1,c1)::t1, [] -> ("1:" + String.replicate c1 (string w1))::fxx (t1, [])
        | [], (w2,c2)::t2 -> ("2:" + String.replicate c2 (string w2))::fxx ([], t2)

    fxx (wc s1,wc s2)
    |> List.sortBy (Seq.item 2)
    |> List.sortBy (Seq.head)
    |> List.sortByDescending String.length
    |> String.concat "/"
