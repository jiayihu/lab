-- Pos & Move

type Pos = (Int, Int)

data Move = North | South | East | West
  deriving Show

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East


-- Shape
data Shape = Circle Float | Rect Float Float
  deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
