--Sigurd Gravning
module Oblig1 where
    import Data.Char

    data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast  | If Ast Ast Ast | Let String Ast Ast | Var String deriving (Eq, Show)

    parseExpr :: String -> (Ast, String)
    parseExpr ('+':xs) = let (e1, r1) = parseExpr xs;
                             (e2, r2) = parseExpr r1 in (Sum e1 e2, r2)
    parseExpr ('*':xs) = let (e1, r1) = parseExpr xs;
                             (e2, r2) = parseExpr r1 in (Mul e1 e2, r2)
    parseExpr ('-':xs) = let (a, b) = parseExpr xs in (Min a, b)
    parseExpr ('(':xs) = let (a, ')':b) = parseExpr xs in (a,b)
    parseExpr ('i':'f':xs) = let (a, rest1) = parseExpr xs;
                                 (b, rest2) = parseExpr rest1;
                                 (c, rest3) = parseExpr rest2;
                             in  (If a b c, rest3)
    parseExpr ('l':'e':'t':xs) = let (a, rest1) = parseHelp xs;
                                     (b, rest2) = parseExpr rest1;
                                     (c, rest3) = parseExpr rest2
                                 in  (Let a b c, rest3)
        where parseHelp :: String -> (String, String)
              parseHelp [] = error "Invalid Variable Name"
              parseHelp (x:xs) | isUpper x = ([x], xs)
                               | otherwise = parseHelp xs
    parseExpr (' ':xs) = parseExpr xs
    parseExpr ('i':'n':xs) = parseExpr xs
    parseExpr ('=':xs) = parseExpr xs
    parseExpr (x:xs) | isUpper x = (Var [x], xs)
                     | isDigit x = (Nr (read (takeWhile isDigit xs)), xs)
                     | otherwise = error $ "Unexpected token " ++ [x]

    parse :: String -> Ast
    parse str = let (a, b) = parseExpr str in a

    --                 type          multiplier        sum             minus       variable     default
    folde :: Eq a => (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> [(String, a)] -> a -> Ast -> a
    folde num mult su mi var def (Nr x) = num x
    folde num mult su mi var def (Mul x y) = mult (folde num mult su mi var def x) (folde num mult su mi var def y)
    folde num mult su mi var def (Sum x y) = su (folde num mult su mi var def x) (folde num mult su mi var def y)
    folde num mult su mi var def (Min x) = mi(folde num mult su mi var def x)
    folde num mult su mi var def (If x y z) = if (folde num mult su mi var def x) == def then (folde num mult su mi var def y) else (folde num mult su mi var def z)
    folde num mult su mi var def (Let x y z) = folde num mult su mi ((x, (folde num mult su mi var def y)):var) def z
    folde num mult su mi var def (Var x) = foldeHelp var x
        where foldeHelp :: [(String, a)] -> String -> a
              foldeHelp [] y = error $ "Unbound variable " ++ y
              foldeHelp (x:xs) y | fst(x) == y = snd(x)
                                 | otherwise = foldeHelp xs y

    evi :: String -> Int
    evi str = folde (id) (*) (+) (0-) [] 0 (parse str)

    evb :: String -> Bool
    evb str = folde (\x -> x `mod` 2 == 1) (&&) (||) (not) [] True (parse str)
