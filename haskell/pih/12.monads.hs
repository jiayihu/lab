type State = Int
newtype ST a = S { runState :: State -> (a, State) }

instance Functor ST where
  fmap g st = S (\s -> let (a, s') = runState st s in (g a, s'))

instance Applicative ST where
  pure a = S (\s -> (a, s))
  stf <*> stx = S (\s -> let
    (f, s') = runState stf s
    (a, s'') = runState stx s'
    in (f a, s''))
    
instance Monad ST where
  st >>= f = S (\s -> let (a, s') = runState st s in runState (f a) s')

{-
State Monad for Tree labelling
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

-- Applicative version
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = fmap Leaf fresh
alabel (Node l r) = pure Node <*> (alabel l) <*> (alabel r)

-- Monadic version
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

{-
Landing on the pole
-}

type Birds = Int
type Pole = (Int, Int)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs ((right + n) - left) < 4 = Just (left, right + n)
  | otherwise = Nothing
