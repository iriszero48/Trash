let computeLargestSum (list:List<int>) =
  if List.forall ((>) 0) list then
    0
  else
    (snd <| List.fold (fun (s,m) x -> let s = List.max [s+x;x] in (s, List.max [m;Some(s)])) (0,None) list).Value
