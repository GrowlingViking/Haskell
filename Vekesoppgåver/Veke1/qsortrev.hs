qsortrev [] = []
qsortrev(x:xs) = qsortrev larger ++ [x] ++ qsortrev smaller
				 where
					larger   = [a | a <- xs, a > x]
					smaller  = [b | b <- xs, b <= x]