summer :: Int -> Int -> Int
summer total 0 = total
summer total num = do nr <- getChar
                      let x = read nr :: Int
                      summer (total + x) (num - 1)

adder :: IO ()
adder = do putStr "How many numbers? "
           num <- getChar
           let summen = summer 0 num
           putStrLn (show summen)
