--4.3
safetail :: [a] -> [a]

--Using coditional expression
safetail xs = if null xs then [] else tail xs

--Using guarded expression
safetail xs | null xs   = []
            | otherwise = tail xs

--Using pattern matching
safetail [] = []
safetail (_:xs) = xs
