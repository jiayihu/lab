module Introduction where

import Prelude hiding (last, init)

double :: Num a => a -> a
double x = x * 2

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]

avarage :: [Int] -> Int
avarage ns = div (sum ns) (length ns)

last :: [a] -> a
last ns = (drop (length ns - 1) ns) !! 0

init :: [a] -> [a]
init ns = take (length ns - 1) ns
