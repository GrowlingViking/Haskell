euclid :: Int -> Int -> Int
euclid a b | a == b    = b
           | a > b     = euclid (a-b) b
           | otherwise = euclid (b-a) a
