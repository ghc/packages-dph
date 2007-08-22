module Data.Array.Parallel.Lifted.PArray (
  PArray, PRepr, PA(..), emptyPA,
  (:*:)(..), (:+:)(..), Embed(..)
) where

import GHC.Exts (Int#)

-- |Lifted parallel arrays
--
data family PArray a

-- |Representation types
--
type family PRepr a

-- |Dictionaries
--
data PA a = PA {
              lengthPA    :: PArray a -> Int#
            , replicatePA :: Int# -> a -> PArray a
            , toPRepr     :: a -> PRepr a
            }

emptyPA :: PA a -> PArray a
emptyPA pa = replicatePA pa 0# (error "PArray.emptyPA: empty")

infixl 2 :*:
infixl 1 :+:

data a :*: b = a :*: b
data a :+: b = Inl a | Inr b

data Embed a = Embed (PA a) a

