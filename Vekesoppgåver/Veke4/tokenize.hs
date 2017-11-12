--Viktig for oblig tokenize ('i':'f':xs)
tokenize :: String -> String -> String -> [String]
tokenize [] imp rm = []
tokenize str@(x:xs) imp rm | elem x imp = [x] : tokenize xs imp rm
                           | elem x rm  = tokenize xs imp rm
                           | otherwise  =
                               let (word, rest) = span(\c -> notElem c imp ++ rm) str
                               in word ++ tokenize rest imp rm
