quad :: Int -> Int -> Int
quad x y | x > 0 && y > 0 = 1
         | x < 0 && y > 0 = 2
         | x < 0 && y < 0 = 3
         | x > 0 && y < 0 = 4

main = do
    x <- readLn
    y <- readLn
    let result = quad x y
    print result
