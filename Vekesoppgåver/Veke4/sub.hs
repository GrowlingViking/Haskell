sub :: Int -> Int -> [a] -> [a]
sub a b xs = [x | (x,i) <- zip xs [0 .. length c-1], i >= a && i < b]
--Eller
sub a b xs = [xs !! n | n <- [a .. b-1]]
