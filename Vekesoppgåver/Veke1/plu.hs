import Test.QuickCheck

plu [] n = []
plu (x:xs) n = [x + n] ++ plu xs n

antiPlu [] n = []
antiPlu (x:xs) n = [x - n] ++ antiPlu xs n

quickCheck (\li -> antiPlu (plu li n) n == li)