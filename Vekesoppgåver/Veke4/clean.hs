import Data.Char (isAlpha, toLower)

clean:: String -> String
clean [] = []
clean (x:xs) | isAlpha x = toLower x:clean xs
             | otherwise = clean xs
--Hale rekursivt
clean xs = clean' xs []
    where
        clean' [] acc = acc
        clean' (x:xs) acc | isAlpha x = clean' xs (acc ++ [toLower x])
                          | otherwise = clean' xs acc
