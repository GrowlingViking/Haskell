import Test.QuickCheck

pali "" = True
pali [x] = True
pali str = if head str == last str then pali (tail (init str) )
			else False
			
makePali s = s ++ reverse s

quickCheck (\li -> pali(makePali li))