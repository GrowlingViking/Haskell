--Sigurd Gravning

main = do
    parse [] []

parse :: History -> Board -> IO ()
parse history board = do
    showcells board
    putStrLn "Enter command"
    input <- getLine
    command = head input
    --parse tall
    case command of  'c' -> do foo <- getChar
                               let n = read [foo] :: Int
                               vis n
                               parse [] []
                     'n' -> do foo <- getChar
                               bar <- getChar
                               let x = read [foo] :: Int
                               let y = read [bar] :: Int
                               parse (board:history) (livingCell (x,y) board)
                     'd' -> do foo <- getChar
                               bar <- getChar
                               let x = read [foo] :: Int
                               let y = read [bar] :: Int
                               --writeat (lft + 3*x, 1 + y) "."
                               parse (board:history) (killCell (x,y) board)
                      otherwise -> do putStrLn "Invalid command!"
                               parse history board

type Pos = (Int, Int)
type Board = [Pos]
type History = [Board]
type Rules = (Survive, Birth)
type Survive = (Int, Int)
type Birth = (Int, Int)

vis :: Int -> IO ()
vis nR = do clr
            writeTop nR
            mapM_ (\i -> writerow i nR) [1..nR]

clr :: IO()
clr = putStr "\ESC[2J"

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

showcells :: Board -> IO ()
showcells b = sequence_ [writeat (lft + 3*x, 1 + y) "X" | (x,y) <- b]

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
