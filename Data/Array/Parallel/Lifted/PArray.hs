module Data.Array.Parallel.Lifted.PArray (
  PArray, Repr, PA(..), emptyPA, Embed, embedPA
) where

import GHC.Exts (Int#)

-- |Lifted parallel arrays
--
data family PArray a

-- |Representation types
--
type family Repr a

-- |Dictionaries
--
data PA a = PA {
              lengthPA    :: PArray a -> Int#
            , replicatePA :: Int# -> a -> PArray a
            }

emptyPA :: PA a -> PArray a
emptyPA pa = replicatePA pa 0# (error "PArray.emptyPA: empty")

-- |Embedding arbitrary types in generic representation
--
data Embed a = Embed (PA a) a

embedPA :: PA a -> a -> Embed a
embedPA = Embed

