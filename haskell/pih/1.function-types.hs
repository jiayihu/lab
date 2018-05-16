module Introduction where

import Prelude hiding (last, init)

double :: Num a => a -> a
double x = x * 2

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]

-- avarage must be defined twice, one for Integral and one for Fractional
-- because `div` is defined twice for both
avarage :: Integral a => [a] -> a
avarage ns = (sum ns) `div` (fromIntegral (length ns))

last :: [a] -> a
last ns = head (reverse ns)

init :: [a] -> [a]
init ns = take (length ns - 1) ns
