let interpret (code: string) =
    let rec inter code value output =
        match code with
        | [] -> output
        | '+'::tail -> inter tail (match value > 254 with | true -> 0 | _ -> value + 1) output
        | '.'::tail -> inter tail value (((string)((char)value))::output)
        | _::tail -> inter tail value output
    (inter (code.ToCharArray() |> List.ofArray) 0 []) |> List.rev |> String.concat ""
