import System.IO
import Data.Char

getLine' :: IO String
getLine' = do 
  x <- getChar
  if x == '\n' then return []
  else do 
    xs <- getLine'
    return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
  putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  putStr (show (length xs))
  putStrLn " characters"

{-
  Hangman
-}

getCh :: IO Char
getCh = do 
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

sGetLine :: IO String
sGetLine = do
  x <- getCh
  if x == '\n' then do
    putChar x
    return []
  else do
    putChar '-'
    xs <- sGetLine
    return (x:xs) 

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

playHangman :: String -> IO ()
playHangman word = do 
  putStr "? "
  guess <- getLine
  if guess == word then putStrLn "You got it!!"
  else do
    putStrLn (match word guess)
    playHangman word

hangman :: IO ()
hangman = do
  putStrLn "Think of a word: "
  word <- sGetLine
  putStrLn "Try to guess it: "
  playHangman word

{-
  Nim
-}

-- Board utilities

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = map update (zip [1..] board)
  where update (r,n) = if r == row then n - num else n

-- IO utilities

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x then return (digitToInt x)
  else do
    putStrLn "ERROR: Invalid digit"
    getDigit prompt

playNim :: Board -> Int -> IO ()
playNim board player = do
  newline
  putBoard board
  if finished board then do
    newline
    putStr "Player "
    putStr (show (next player))
    putStrLn " wins!"
  else do
    newline
    putStr "Player "
    putStrLn (show player)
    row <- getDigit "Enter a row number: "
    num <- getDigit "Stars to remove: "
    if valid board row num then
      playNim (move board row num) (next player)
    else do
      newline
      putStrLn "ERROR: Invalid move"
      playNim board player

nim :: IO ()
nim = playNim initial 1

main :: IO ()
main = nim
