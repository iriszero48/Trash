a = [sin, cos, flip (**) 3.0]
b = [asin, acos, flip (**) $ 1 / 3]
c = zipWith (.) a b
print $ map (\x -> x 0.5) c
