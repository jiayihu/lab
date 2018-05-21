{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (a, w)}

instance Monad Identity where
  return = Identity
  -- (>>=) :: m a -> (a -> m b) -> m b
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State (\s -> (x, s))
  (State g) >>= f = State (\s -> let (a, s') = g s in runState (f a) s')

instance Monad (Reader s) where
  return x = Reader (\_ -> x)
  (Reader g) >>= f = Reader (\s -> let a = g s in runReader (f a) s)

instance (Monoid w) => Monad (Writer w) where
  return w = Writer (w, mempty)
  (Writer (a, w)) >>= f = let Writer (fa, fw) = f a in Writer (fa, w `mappend` fw)

