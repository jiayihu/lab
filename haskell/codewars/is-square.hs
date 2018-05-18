{-
  Given an integral number, determine if it's a square number

  is_square (-1) # => false
  is_square   0 # => true
  is_square   3 # => false
  is_square   4 # => true
  is_square  25 # => true
  is_square  26 # => false
-}

import Prelude hiding ((^))
import qualified Prelude ((^))

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

isSquare :: Integral n => n -> Bool
isSquare n
    | n < 0 = False
    | otherwise = not $ null [x | x <- [0..n], x^2 == n]
