let rec QuickSort = function
    | h::t ->
        let l, r = List.partition ((>=) h) t
        in (QuickSort l) @ (h::QuickSort r)
    | _ -> []
