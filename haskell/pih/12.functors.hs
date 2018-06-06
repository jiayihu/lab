{- Exercises of the chapter -}

-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

tree :: Tree Char
tree = Node (Node Leaf 'a' Leaf) 'b' Leaf

-- 2
-- instance Functor ((->) a) where
--   fmap g f = g . f

-- 3
-- instance Applicative ((->) a) where
--   pure x = \_ -> x
--   f <*> g = \x -> f x (g x)

-- 6
-- instance Monad ((->) a) where
--   h >>= f = (\x -> f (h x) x)

-- 4

newtype ZipList a = Z [a]
  deriving Show

instance Functor ZipList where
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure x = Z (repeat x)
  (Z gs) <*> (Z xs) = Z [g x |(g, x) <- zip gs xs]

list :: ZipList Int
list = Z [1, 2, 3]

-- 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

expr = Add (Var 1) (Val 2)

instance Functor Expr where
  fmap g (Var a) = Var (g a)
  fmap g (Val a) = Val a
  fmap g (Add a b) = Add (fmap g a) (fmap g b)

instance Applicative Expr where
  pure x = Var x
  _       <*> Val x   = Val x
  Val x   <*> _       = Val x
  (Var f) <*> e = fmap f e
  Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
  Var a >>= f = f a
  Val a >>= f = Val a
  (Add a b) >>= f = Add (a >>= f) (b >>= f)

-- 8
type State = Int
newtype ST a = S { runState :: State -> (a, State) }

instance Functor ST where
  fmap g st = do
    x <- st
    return (g x)

instance Applicative ST where
  pure x = S (\s -> (x, s))
  stf <*> stx = do
    f <- stf
    x <- stx
    return (f x)

instance Monad ST where
  st >> f = S (\s -> let (x, s') = runState st s in runState (f x) s')
