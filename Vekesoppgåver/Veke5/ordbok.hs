import Data.Char
import Data.List

type Ordbok = [(String, Int)]

boka = [("eple",1), ("pære",2), ("banan",3), ("appelsin",4), ("kiwi",5)]
boka2 = reverse boka

finn :: String -> Ordbok -> Int
finn n bok = head [v | (n',v) <- bok, n == n']

settInn :: String -> Int -> Ordbok -> Ordbok
settInn n v bok = (n,v):bok

endre :: String -> Int -> Ordbok -> Ordbok
endre n v [] = []
endre n v (b:bok) = if (fst b) == n then settInn n v (slettF n (b:bok)) else b:endre n v bok

slettF :: String -> Ordbok -> Ordbok
slettF n (b:bok) = if (fst b) == n then bok else b:slettF n bok

slettAlle :: String -> Ordbok -> Ordbok
slettAlle n (b:bok) = if (fst b) == n then slettAlle n bok else b:slettAlle n bok

erLike :: Ordbok -> Ordbok -> Bool
erLike [] bok2 = True
erLike (b1:bok1) bok2 = if elem b1 bok2 then erlike bok1 bok2 else False
