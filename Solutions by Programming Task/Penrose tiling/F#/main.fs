open System.Windows
open System.Windows.Media.Imaging
open System.Collections.Generic
open System.Windows.Media
open System
open System.Drawing

type Type = Kite | Dart

type Tile(typ,x,y,angle,size) =
    member this.Type = typ
    member this.X = x
    member this.Y = y
    member this.Angle = angle
    member this.Size = size

let Convert (bitmap:Bitmap) =
    let bitmapData = bitmap.LockBits(new System.Drawing.Rectangle(0, 0, bitmap.Width, bitmap.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, bitmap.PixelFormat)
    let bitmapSource = BitmapSource.Create(bitmapData.Width, bitmapData.Height, float(bitmap.HorizontalResolution), float(bitmap.VerticalResolution), PixelFormats.Bgr32, null, bitmapData.Scan0, bitmapData.Stride * bitmapData.Height, bitmapData.Stride)
    bitmap.UnlockBits(bitmapData)
    bitmapSource

[<System.STAThread>]
do
    let w = 3840
    let h = 2160

    let ToRadians d = d * (Math.PI * 1.) / 180.0
    let G =  (1. + sqrt 5.) / 2.
    let T = ToRadians(36.)
    
    let proto = new List<Tile>()
    for x in [Math.PI / 2. + T .. 2. * T .. 3. * Math.PI] do
        proto.Add(Tile(Kite, float(w)/2., float(h)/2., x, float(w)/2.5))

    let rec deflateTiles (tiles:List<Tile>) = function
        | 0 -> tiles
        | x ->
            let next = new List<Tile>()
            for tl in tiles do
                let x = tl.X
                let y = tl.Y
                let a = tl.Angle
                let size = tl.Size / G
                match tl.Type with
                | Dart ->
                    next.Add(Tile(Kite, x, y, a + 5. * T, size))
                    for sign in [ 1.; -1. ] do
                        next.Add(Tile(Dart, x + cos(a - 4. * T * sign) * G * tl.Size, y - sin(a - 4. * T * sign) * G * tl.Size, a - 4. * T * sign, size))
                | _ ->
                    for sign in [ 1.; -1. ] do
                        next.Add(Tile(Dart, x, y, a - 4. * T * sign, size))
                        next.Add(Tile(Kite, x + cos(a - T * sign) * G * tl.Size, y - sin(a - T * sign) * G * tl.Size, a + 3. * T * sign, size))
            deflateTiles next (x - 1)

    let tiles = deflateTiles proto 5
    let bmp = new Bitmap(w, h)
    use g = Graphics.FromImage(bmp)
    let dist = [| [|G;G;G|]; [|-G;-1.;-G|] |]
    for tl in tiles do
        let start = [| PointF(float32 tl.X, float32 tl.Y) |]
        let ord = match tl.Type with | Kite -> 0 | _ -> 1
        let move = Array.init 3 (fun i -> tl.Angle - T + T * float(i)) |> Array.mapi (fun i angle ->
            PointF(float32(tl.X + dist.[ord].[i] * tl.Size * cos angle), float32(tl.Y - dist.[ord].[i] * tl.Size * sin angle)))
        let points = Array.concat [| start; move |]
        g.FillPolygon((match tl.Type with | Kite -> Brushes.Orange | _ -> Brushes.Yellow), points)
        g.DrawPolygon(Pens.DarkGray, points)
        
    let image = Controls.Image(Stretch=Media.Stretch.Uniform)
    image.Source <- Convert bmp
    Window(Content=image, Title="Penrose tiling")
    |> (Application()).Run |> ignore
