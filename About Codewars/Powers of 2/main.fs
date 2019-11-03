let powersOfTwo n = [0..n]|>List.map (fun x -> System.Convert.ToInt32((System.Math.Pow(2.0,(float)x))))
