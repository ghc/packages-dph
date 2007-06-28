module Data.Array.Parallel.Lifted.PArray (
  PArray, PA(..)
) where

-- |Lifted parallel arrays
--
data family PArray a

-- |Dictionaries
--
data PA a = PA {
              lengthP    :: PArray a -> Int
            , replicateP :: Int -> a -> PArray a
            }

