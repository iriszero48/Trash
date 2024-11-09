// let a = Array.init (128*128) (fun i -> float(i) / float(128*128))
let a = Array.init (128*128) (fun _ -> (new System.Random(System.DateTime.Now.Millisecond)).NextDouble())
a.[128*128-1]<-1.0
let b = Array.create (128*128) 1.0
let c = Array.create (128*128) 0.0

let Add (m : float array) x = 
    for i = 0 to (128 * 128) - 1 do
        m.[i] <- m.[i] + x

let Div (m : float array) x =
    for i = 0 to (128 * 128) - 1 do
        m.[i] <- m.[i] / x

let Mul (a : float array) (b : float array) (c : float array) =
    for row = 0 to 128 - 1 do
        for col = 0 to 128 - 1 do
            let mutable v = 0.0
            for i = 0 to 128 - 1 do
                v <- v + a.[row * 128 + i] * b.[i * 128 + col]
            c.[row * 128 + col] <- v

let tp1 = System.DateTime.Now

for i = 1 to 1000 do
    Add b (float(i) - 1.0)
    Mul a b c

    Add b (float(i))
    Mul c b a

    Div a (Array.get c (127 * 128 + 127))

let sumValue = Array.sum a

let tp2 = System.DateTime.Now

printfn $"{(tp2.Subtract(tp1).TotalNanoseconds / 1000.0 / 1000.0)} {sumValue}"
