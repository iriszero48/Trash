let nb_months (startPriceOld: float) (startPriceNew: float) (savingperMonth: float) (percentLossByMonth: float) =
    let rec fxx times percent startPriceOld startPriceNew saving =
        let x = startPriceOld +. saving -. startPriceNew in
        if x >= 0. then (times, int_of_float (floor (x +. 0.5)))
        else fxx (times + 1) (if (times land 1) = 0 then percent -. 0.005 else percent) (startPriceOld *. percent) (startPriceNew *. percent) (saving +. savingperMonth)
    in fxx 0 (1. -. percentLossByMonth /. 100.) startPriceOld startPriceNew 0.
