let operArray fct arr init = List.scan fct init arr |> List.skip 1

let som = (+)

let mini x y = List.min [x; y]

let maxi x y = List.max [x; y]

let rec gcdi x y = if y = 0 then abs x else gcdi y (x % y)

let lcmu x y = abs (x * y) / (gcdi x y)
