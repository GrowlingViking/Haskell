tegnpos :: Char -> String -> [Int]
tegnpos a xs = [y | y <- [0 .. length xs-1], xs !! y == a]
