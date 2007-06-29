module Data.Array.Parallel.Lifted.PArray (
  PArray, PA(..)
) where

-- |Lifted parallel arrays
--
data family PArray a

-- |Dictionaries
--
data PA a = PA {
              lengthPA    :: PArray a -> Int
            , replicatePA :: Int -> a -> PArray a
            }

