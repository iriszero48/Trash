open System
open System.Windows
open System.Windows.Media.Imaging

let rec skipPrevious p c i =
    match p,i with
    | ']'::t, i -> skipPrevious t (']'::c) (i + 1) 
    | '['::t, 0 -> t, '['::c
    | '['::t, i -> skipPrevious t ('['::c) (i - 1)
    | h::t, i -> skipPrevious t (h::c) i

let rec skipNext p c i =
    match c,i with
    | '['::t, i -> skipNext ('['::p) t (i + 1)
    | ']'::t, 0 -> ']'::p, t
    | ']'::t, i -> skipNext (']'::p) t (i - 1)
    | h::t, i -> skipNext (h::p) t i

[<STAThread>]
[<EntryPoint>]
let main argv =
    let code = "*
    [[s*en]
    sw[w]enn
    [[e]w*ssss*nnnnw[w]ess[e]*[w]enn]
    ssss[*nnnn*essss]
    nnnnw[w]e
    ss]"

    let width = 1000
    let height = 500

    let image = Controls.Image(Stretch=Media.Stretch.Uniform)
    let format = Media.PixelFormats.Gray2
    let pixel = Array.create (width*height) 0uy
    let data:bool[,] = Array2D.zeroCreate height width

    let mutable x_ = 0
    let mutable y_ = 0
    let mutable pn_ = [], code.ToCharArray() |> Array.toList

    let rec fxx x y = function
        | (_, [])                -> ()
        | (p, h::t) when h = 'n' -> fxx ((x + height - 1) % height) y                         (h::p, t)
        | (p, h::t) when h = 'e' -> fxx x                           ((y + 1) % width)         (h::p, t)
        | (p, h::t) when h = 's' -> fxx ((x + 1) % height)          y                         (h::p, t)
        | (p, h::t) when h = 'w' -> fxx x                           ((y + width - 1) % width) (h::p, t)
        | (p, h::t) when h = '*' ->
            data.[x,y] <- not data.[x,y]

            data |> Seq.cast<bool> |> Seq.iteri (fun i x -> pixel.[i] <- if x then 255uy else 0uy)
            image.Source <- BitmapSource.Create(width, height, 1.0, 1.0, format, null, pixel, width)

            x_ <- x
            y_ <- y
            pn_ <- h::p, t
        | (p, h::t) when h = '[' && data.[x,y] -> fxx x y (h::p, t)
        | (p, h::t) when h = '[' -> fxx x y (skipNext (h::p) t 0)
        | (p, h::t) when h = ']' -> fxx x y (skipPrevious p (h::t) 0)
        | (p, h::t) -> fxx x y (h::p, t)

    Media.CompositionTarget.Rendering.Add (fun _ -> fxx x_ y_ pn_)

    Window(Content=image, Title="Paintfuck") |> (Application()).Run |> ignore
    0
