module Combinations where

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [ x:c | c <- combinations (n-1) xs] ++ (combinations n xs)
