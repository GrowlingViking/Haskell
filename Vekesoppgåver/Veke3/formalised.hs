og :: Bool -> Bool -> Bool

--4.5
og x y = if x then
    if y then
        True
    else
        False
else
    False

--4.6
og x y = if x then
    y
else
    False

--4.7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x*y*z ) )
