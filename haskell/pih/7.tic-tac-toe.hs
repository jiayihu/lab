import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

data Player = O | B | X
  deriving (Eq, Ord, Show)

type Grid = [[Player]]

-- Utilities

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y:xs) = y : x : interleave x xs

next :: Player -> Player
next O = X
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where 
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)] 

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size -1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- IO

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
      beside = foldr1 (zipWith (++)) -- `zipWith` is like reading the matrix by column
      bar = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4) - 1) '-']

