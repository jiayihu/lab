module HeathrowToLondon
( Section(..)
, Label(..)
, optimalPath
) where

{-
        |                                   |
       A|___50___|___05___|___40___|___10___|
AIRPORT |      30|      20|      25|        |  LONDON
       B|___10___|___90___|___02___|___08___|
        |                                   |
        
10+30 40+05     45+40 67+08
10    40+05+20  65+02 10+30+05+65+02+08 = 75

        |                                   |
       A|___50___|__110___|____0___|___04___|
AIRPORT |     100|      20|      30|        |  LONDON
       B|___100__|___60___|___15___|___02___|
        | 

 50 50+110 50+110    50+110+04
100 100+60 100+65+15 50+110+04
-}

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Eq, Show)  
type RoadSystem = [Section]

data Label = A | B | C deriving (Eq, Show)  
type Path = [(Label, Int)]

bestPath :: Section -> (Label, Int) -> Path
bestPath x (l, _) 
  | l == A = if a < bc then [(A, a)] else [(B, b), (C, c)]
  | l == B = if ac < b then [(A, a), (C, c)] else [(B, b)]
  | otherwise = if a < b then [(A, a)] else [(B, b)] -- C is the next label
  where
    a = getA x
    b = getB x
    c = getC x
    ac = a + c
    bc = b + c

optimalPath :: RoadSystem -> Path
optimalPath [] = [(C, 0)]
optimalPath (x:xs) = (bestPath x y) ++ (y:ys)
  where
    (y:ys) = optimalPath xs
