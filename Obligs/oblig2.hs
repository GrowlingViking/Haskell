--Sigurd Gravning
import Data.Char
import System.Exit

main = do
    parse [] [] 0 ((2,3),(3,3)) ""
                           --Size          --Message
parse :: History -> Board -> Int -> Rules -> String -> IO ()
parse history board size rules message = do
    vis size
    showcells board size
    if elem board history then putStrLn "Stable configuration achieved!" else putStrLn ""
    putStrLn message
    putStrLn "Enter command"
    command <- getLine
    case command of ('c':' ':xs) -> if all isDigit xs then do let n = parseNum xs
                                                              parse [] [] n rules ""
                                                              else parse [] [] 0 rules "Input not a number or too may numbers!"
                    ('n':' ':xs) -> do let ints = words xs
                                       if all (\a -> all isDigit a) ints then do let (x,y) = parseNumbers ints
                                                                                 if (x > size || y > size) then parse history board size rules "Input number to high!"
                                                                                 else parse (history) (livingCell (x,y) board) size rules ""
                                                                                 else parse history board size rules "Input not a number!"
                    ('d':' ':xs) -> do let ints = words xs
                                       if all (\a -> all isDigit a) ints then do let (x,y) = parseNumbers ints
                                                                                 if (x > size || y > size) then parse history board size rules "Input number to high!"
                                                                                 else parse (history) (killCell (x,y) board) size rules ""
                                                                                 else parse history board size rules "Input not a number!"
                    ('s':' ':xs) -> do let ints = words xs
                                       if all (\a -> all isDigit a) ints then do let (x,y) = parseNumbers ints
                                                                                 parse history board size (sRules (x,y) rules) ""
                                                                                 else parse history board size rules "Input not a number!"
                    ('b':' ':xs) -> do let ints = words xs
                                       if all (\a -> all isDigit a) ints then do let (x,y) = parseNumbers ints
                                                                                 parse history board size (bRules (x,y) rules) ""
                                                                                 else parse history board size rules "Input is not a number!"
                    ('l':' ':xs) -> do if all isDigit xs then do let x = parseNum xs
                                                                 life history board size rules x False
                                                                 else parse history board size rules "Input not a number or too many numbers!"
                    "p" -> parse (tail history) (head history) size rules ""
                    ('w':' ':xs) -> do writeFile xs (parseOut rules size board)
                                       parse history board size rules "Saved!"
                    ('r':' ':xs) -> do file <- readFile xs
                                       parseFile (words file)
                    "" -> parse (board:history) (checkBoard (nextgen board rules) size) size rules ""
                    "?" -> parse history board size rules (parseRules rules)
                    "q" -> return ()
                    otherwise -> parse history board size rules "Invalid command!"

type Pos = (Int, Int)
type Board = [Pos]
type History = [Board]
type Rules = (Survive, Birth)
type Survive = (Int, Int)
type Birth = (Int, Int)

life :: History -> Board -> Int -> Rules -> Int -> Bool -> IO ()
life history board size rules 0 stable = parse history board size rules "Done!"
life history board size rules x True = parse history board size rules ("Achieved after " ++ (show x) ++ " turns")
life history board size rules x False = do vis size
                                           showcells board size
                                           wait 500000
                                           if elem board history then life (board:history) (checkBoard (nextgen board rules) size) size rules x True
                                           else life (board:history) (checkBoard (nextgen board rules) size) size rules (x - 1) False

parseNum :: String -> Int
parseNum [] = 0
parseNum xs = read (takeWhile isDigit xs)

parseNumbers :: [String] -> (Int, Int)
parseNumbers [] = (0, 0)
parseNumbers [x] = (0, 0)
parseNumbers (x:y:xs) = ((parseNum x), (parseNum y))

parseFile :: [String] -> IO ()
parseFile file = do let sx = read (file !! 1) :: Int
                    let sy = read (file !! 2) :: Int
                    let bx = read (file !! 4) :: Int
                    let by = read (file !! 5) :: Int
                    let size = read (file !! 6) :: Int
                    let board = getBoard (drop 7 file)
                    parse [] board size ((sx,sy),(bx,by)) "Game loaded!"

getBoard :: [String] -> Board
getBoard [] = []
getBoard (x:[]) = []
getBoard (x:y:xs) = ((read x :: Int),(read y :: Int)):(getBoard xs)

parseRules :: Rules -> String
parseRules ((sx,sy),(bx,by)) = "Surviving from " ++ (show sx) ++ " to " ++ (show sy) ++ ", Births from " ++ (show bx) ++ " to " ++ (show by)

parseOut :: Rules -> Int -> Board -> String
parseOut ((sx,sy), (bx,by)) size board = "s " ++ (show sx) ++ " " ++ (show sy) ++ " b " ++ (show bx) ++ " " ++ (show by) ++ " " ++ (show size) ++ " " ++ parseBoard board

parseBoard :: Board -> String
parseBoard [] = ""
parseBoard (x:xs) = parsePos x ++ parseBoard xs

parsePos :: Pos -> String
parsePos (x,y) = (show x) ++ " " ++ (show y) ++ " "

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
