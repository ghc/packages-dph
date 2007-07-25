module Data.Array.Parallel.Lifted.PArray (
  PArray, PA(..), emptyPA
) where

import GHC.Exts (Int#)

-- |Lifted parallel arrays
--
data family PArray a

-- |Dictionaries
--
class PA a where
  lengthPA    :: PArray a -> Int#
  replicatePA :: Int# -> a -> PArray a

emptyPA :: PA a => PArray a
emptyPA = replicatePA 0# (error "PArray.emptyPA: empty")

