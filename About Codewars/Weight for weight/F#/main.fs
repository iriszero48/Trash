open System.Text.RegularExpressions;;

let orderWeight(s: string): string =
  Regex.Replace(s.Trim(), " +", " ").Split(' ')
  |> Seq.sort
  |> Seq.sortBy (fun x ->
    x.ToCharArray()
    |> Seq.map (int >> (+) -48)
    |> Seq.sum)
  |> String.concat " "
