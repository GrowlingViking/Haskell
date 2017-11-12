fjern :: String -> Char -> String
fjern xs y = [x | x <- xs, x /= y]
--Bruk filter viss ikkje listekomprehensjon
