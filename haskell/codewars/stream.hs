module Stream where

import Control.Arrow
import Control.Applicative

data Stream a = Empty | a :> Stream a

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (a :> _) = a

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> s) = s


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

iterS :: (a -> a) -> a -> Stream a
iterS f a = fa :> (iterS f fa)
    where fa = f a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f a = a :> (iterS f a)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = foldr (\x stream -> x :> stream) (cycleS xs) xs

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS a = a :> (fromS $ a+1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS a s = a :> (fromStepS (a+s) s)

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs)
    | p x = x :> (filterS p xs)
    | otherwise = filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS i (x :> xs) 
    | i > 0 = x : (takeS (i-1) xs)
    | otherwise = []

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS 0 s = s
dropS i (x :> xs) 
    | i > 0 = dropS (i-1) xs
    | otherwise = x :> xs

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS 0 s = ([], s)
splitAtS i (x :> xs) = (x:ls, rs)
    where 
        (ls, rs) = splitAtS (i-1) xs

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = (f x y) :> (zipWithS f xs ys)

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = (f x) :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = repeatS

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (f :> fs) <*> (x :> xs) = (f x) :> (fs <*> xs)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> (1 :> (f 0 1))
    where f x y = (x+y) :> (f y (x+y))

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = cycleS primeList
    where
        primeList = [x | x <- [2..], isPrime x]
        isPrime x = null [d | d <- [2..x-1], x `mod` d == 0]


