-- | Given a list, find the [Int] that appears an 
--   odd number of times. The tests will always
--   provide such a number, and the list will
--   always contain at least one element.

import Data.List

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile p (x:xs) 
  | p x = (x:taken, dropped)
  | otherwise = ([], x:xs)
  where (taken, dropped) = takeDropWhile p xs

findOdd' :: [Int] -> Int
findOdd' xs
  | odd $ length occurs = number
  | otherwise = findOdd' left
  where
    number = head $ xs
    (occurs, left) = takeDropWhile (== number) xs

findOdd :: [Int] -> Int
findOdd = findOdd' . sort
