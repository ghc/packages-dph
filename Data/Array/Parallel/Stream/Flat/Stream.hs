module Data.Array.Parallel.Stream.Flat.Stream (
  Step(..), Stream(..)
) where

data Step s a = Done
              | Skip     !s
              | Yield !a !s

instance Functor (Step s) where
  fmap f Done        = Done
  fmap f (Skip s)    = Skip s
  fmap f (Yield x s) = Yield (f x) s

data Stream a = forall s. Stream (s -> Step s a) !s Int

