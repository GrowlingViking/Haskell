--Sigurd Gravning
import Data.Char
import System.Exit

main = do
    parse [] [] 0 ((2,3),(3,3)) ""
                           --Size
parse :: History -> Board -> Int -> Rules -> String -> IO ()
parse history board size rules message = do
    vis size
    showcells board size
    putStrLn message
    if elem board history then putStrLn "Stable configuration achieved!" else putStrLn ""
    putStrLn "Enter command"
    command <- getLine
    case command of ('c':' ':xs) -> do let n = parseNum xs
                                       vis n
                                       parse [] [] n rules ""
                    ('n':' ':xs) -> do let x = parseNum xs
                                       let y = parseNum (tail (dropWhile isDigit xs))
                                       if (x <= size || y <= size) then parse (board:history) (livingCell (x,y) board) size rules ""
                                           else parse history board size rules "Input number to high"
                    ('d':' ':xs) -> do let x = parseNum xs
                                       let y = parseNum (tail (dropWhile isDigit xs))
                                       if (x <= size || y <= size) then parse (board:history) (killCell (x,y) board) size rules ""
                                           else parse history board size rules "Input number to high"
                    ('s':' ':xs) -> do let x = parseNum xs
                                       let y = parseNum (tail (dropWhile isDigit xs))
                                       parse history board size (sRules (x,y) rules) ""
                    ('b':' ':xs) -> do let x = parseNum xs
                                       let y = parseNum (tail (dropWhile isDigit xs))
                                       parse history board size (bRules (x,y) rules) ""
                    "" -> parse (board:history) (checkBoard (nextgen board rules) size) size rules ""
                    "?" -> parse history board size rules (parseRules rules)
                    "q" -> exitSuccess
                    otherwise -> parse history board size rules "Invalid command!"

type Pos = (Int, Int)
type Board = [Pos]
type History = [Board]
type Rules = (Survive, Birth)
type Survive = (Int, Int)
type Birth = (Int, Int)

parseNum :: String -> Int
parseNum [] = 0
parseNum xs = read (takeWhile isDigit xs)

parseRules :: Rules -> String
parseRules ((sx,sy),(bx,by)) = "Surviving from " ++ (show sx) ++ " to " ++ (show sy) ++ ", Births from " ++ (show bx) ++ " to " ++ (show by)

vis :: Int -> IO ()
vis 0 = return ()
vis nR = do clr
            writeTop nR
            mapM_ (\i -> writerow i nR) [1..nR]

clr :: IO()
clr = putStr "\ESC[2J"

checkBoard :: Board -> Int -> Board
checkBoard b s = [p | p <- b, checkPos p s]

checkPos :: Pos -> Int -> Bool
checkPos (x,y) s = if x <= s && y <= s then True else False

sRules :: Survive -> Rules -> Rules
sRules survive (s,b) = (survive, b)

bRules :: Birth -> Rules -> Rules
bRules birth (s,b) = (s, birth)

writeTop :: Int -> IO ()
writeTop nR = writeat (lft + 1, 0)
    ((concat [(show i) ++ if i < 10 then "  " else " " | i <- [1..nR]]) ++ "\n")

lft = 4

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writerow :: Int -> Int -> IO ()
writerow i nR = do
    writeat (if i > 9 then (lft - 3) else lft - 2, 1 + i) (show i)
    mapM_ (\i -> putStr "  .") [1..nR]
    putStrLn ""

showcells :: Board -> Int -> IO ()
showcells [] n = return ()
showcells b size = do sequence_ [writeat ((lft - 2) + 3*x, 1 + y) "X" | (x,y) <- b]
                      goto (0, size + 3)

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = [(x-1, y-1), (x, y-1),
                 (x+1, y-1), (x-1, y),
                 (x+1, y), (x-1, y+1),
                 (x, y+1), (x+1, y+1)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> Survive -> [Pos]
survivors b (x,y) = [p | p <- b, elem (liveneighbs b p) [x,y]]

births :: Board -> Birth -> [Pos]
births b (x,y) = [p | p <- rmdups (concat (map neighbs b)),
                 isEmpty b p,
                 elem (liveneighbs b p) [x,y]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Rules -> Board
nextgen b (survive,birth) = (survivors b survive) ++ (births b birth)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

livingCell :: Pos -> Board -> Board
livingCell p b = if notElem p b then p:b else b

killCell :: Pos -> Board -> Board
killCell p b = filter (/= p) b
