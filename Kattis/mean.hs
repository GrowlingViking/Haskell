import Data.Char

mean :: (Int, Int) -> Int
mean (x, y) = 2*y - x

parse :: String -> (Int, Int)
parse str = let (first, x:xs) = span isDigit str in (read first, read xs)

main = do
    str <- getLine
    putStrLn $ show $ mean $ parse str
