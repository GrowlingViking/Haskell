import Data.Char

diff :: (Int, Int) -> Int
diff (x, y) = if x > y then x - y else y - x

parse :: String -> (Int, Int)
parse str = let (first, x:xs) = span isDigit str in (read first, read xs)

forA :: [String] -> IO ()
forA [] = return ()
forA (x:xs) = do
    putStrLn $ show $ diff $ parse x
    forA xs

main = do
    content <- getContents
    let fileLines = lines content
    forA fileLines
