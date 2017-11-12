--Først
f :: Int -> Int
f 0 = 0
f x = x^2 + f (x - 1)

--So med liste-komprehesjon
