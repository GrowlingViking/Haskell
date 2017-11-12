--main = do

vis nR = do clr
            writeTop nR
            mapM (\i -> writerow i nR) [1..nR]

clr = putStr "\ESC[2J"

writeTop nR = writeat (lft + 1, 0)
      ((concat [(show (mod i 10)) ++ " " | i <- [1..nR]]) ++ "\n")

lft = 3

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writerow i nR = do
  writeat (if i > 9 then (lft - 2) else lft - 1, 1 + i) (show i)
  mapM (\i -> putStr " .") [1..nR]
  putStrLn ""

type Pos = (Int, Int)

type Board = [Pos]
