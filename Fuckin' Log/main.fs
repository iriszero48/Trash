let FuckingLog x = -1. * List.sumBy (fun _i -> let i = float(_i) in (1.-x) ** i / i) [1 .. 10] // 0 < x < 1
