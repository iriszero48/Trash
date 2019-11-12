let t = [3;4;5]
seq{
    for a in t do
        for b in t do
            for c in t do
                for d in t do
                    for e in t do
                        for f in t do
                            for g in t do
                                yield a::b::c::d::e::f::g::[]
}
|> Seq.filter (fun x ->
    let rec j = function
        | a::b::_ when a = b -> false
        | _::_::[] -> true
        | _::b::tail -> j (b::tail)
    j x)
|> Seq.length
|> System.Console.WriteLine
