import Prelude hiding (and, concat, replicate, (!!), elem)

and :: [Bool] -> Bool
and [x] = x
and (x:xs) = x && and xs
--Hale rekursivt
--ToDo

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs
--Hale rekursivt
concat xs = foldl (++) [] xs
--Eller
concat xs = concat' xs []
concat' (x:xs) acc = concat' xs (acc ++ x)

replicate :: Int -> a -> [a]
replicate 0 x = [x]
replicate n x = x : replicate(n-1) x
--Hale rekursivt

(!!) :: [a] -> Int -> a
(!!) [] n = error "Empty"
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) | a == x = True
              | otherwise elem a xs
