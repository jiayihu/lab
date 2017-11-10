{-
  Life
-}

cls :: IO ()
cls = putStr "\ESC[2J"

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

type Pos = (Int, Int)
type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

width :: Int
width = 10

height :: Int
height = 10

showcells :: Board -> IO ()
showcells b = mapM_ (\p -> writeat p "O") b

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap 
  [(x - 1, y - 1), (x, y - 1),
   (x + 1, y - 1), (x - 1, y),
   (x + 1, y), (x - 1, y - 1),
   (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = filter (\p -> elem (liveneighbs b p) [2, 3]) b

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
  if (length $ survivors b) == 0 then do
    return ()
  else do
    cls
    showcells b
    _ <- getLine
    life (nextgen b)

main :: IO ()
main = life glider
