parse :: String -> [String]
parse [] = []
parse (x:xs) | x == '-' = [x] : parse xs
             | otherwise = parse xs

main = do
    foo <- getLine
    nums <- getLine
    putStrLn $ show $ length $ parse nums
