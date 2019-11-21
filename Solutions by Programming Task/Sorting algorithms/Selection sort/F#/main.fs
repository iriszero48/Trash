let rec SelectionSort = function
    | h::t ->
        let min, tail = List.fold (fun (min, acc) x -> if x < min then (x, min::acc) else (min, x::acc)) (h, []) t
        in min::SelectionSort tail
    | _ -> []
