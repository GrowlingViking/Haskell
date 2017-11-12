add1 = do
    putStr "How many numbers? "
    nrStr <- getLine
    let n = read nrStr :: Int
    nums <- sequence [ getLine | _ <- [1..n] ]
    print $ sum [ read i :: Int | i <- nums ]

add2 = do
    putStr "Write numbers (finish with 0): "
    aux 0

aux x = do
    nr <- getLine
    let n = read nr :: Int
    if (n /= 0) then
        aux (n + x)
    else print x

add3 = do
    putStr "Write numbers (finish with CR): "
    str <- getLine
    let strs = words str
    print ( sum (map (read :: String -> Int ) strs ) )
