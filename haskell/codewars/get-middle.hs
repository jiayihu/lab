{-
You are going to be given a word. Your job is to return the middle character of the word.
If the word's length is odd, return the middle character. If the word's length is even,
return the middle 2 characters.

Kata.getMiddle("test") should return "es"

Kata.getMiddle("testing") should return "t"

Kata.getMiddle("middle") should return "dd"

Kata.getMiddle("A") should return "A"
-}

getMiddle :: String -> String
getMiddle s
  | even l = [(s !! (middle - 1)), (s !! (middle))]
  | otherwise = [s !! (middle)]
  where
    l = length s
    middle = l `div` 2
