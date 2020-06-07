open System.IO
open System

module RandomNumber =
    open System.Security.Cryptography

    let RandomBytes len =
        let csp = new RNGCryptoServiceProvider()
        let buf = Array.zeroCreate len
        csp.GetBytes buf
        csp.Dispose()
        buf

    let RandomUint64 min max =
        let uint64MaxValue = System.UInt64.MaxValue
        let getRandomUint64 = fun _ -> System.BitConverter.ToUInt64(RandomBytes 8, 0)
        let distance = lazy(max - min + 1UL)
        
        if min = max then min
        elif min = 0UL && max = uint64MaxValue then getRandomUint64()
        elif distance.Value &&& distance.Value - 1UL = 0UL then getRandomUint64() % distance.Value + min
        else
            let limit = uint64MaxValue - uint64MaxValue % distance.Value - 1UL in
            let result = Seq.initInfinite getRandomUint64 |> Seq.find ((>) limit) in
            result % distance.Value + min

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 ->
        let data = File.ReadAllLines (Array.exactlyOne argv) in
        Console.WriteLine(data.[int32(RandomNumber.RandomUint64 0UL (uint64(data.Length - 1)))])
    | _ -> Console.Error.WriteLine("Usage: main TextFile")
    Console.ReadLine() |> ignore
    0
