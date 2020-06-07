type RandomNumber() =
    let csp = new System.Security.Cryptography.RNGCryptoServiceProvider()

    member this.RandomBytes len =
        let buf = Array.zeroCreate len
        csp.GetBytes buf
        buf

    member this.RandomUint64 min max =
        let uint64MaxValue = System.UInt64.MaxValue
        let getRandomUint64 = fun _ -> System.BitConverter.ToUInt64(this.RandomBytes 8, 0)
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
    let rn = new RandomNumber()
    Seq.init 10000 (fun _ -> async{ return rn.RandomUint64 0UL 100UL })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.countBy (fun x -> x)
    |> Seq.sort
    |> Seq.iter (printfn "%A")
    0
