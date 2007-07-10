module Data.Array.Parallel.Lifted.PArray (
  PArray, PA(..)
) where

-- |Lifted parallel arrays
--
data family PArray a

-- |Dictionaries
--
class PA a where
  lengthPA    :: PArray a -> Int
  replicatePA :: Int -> a -> PArray a

