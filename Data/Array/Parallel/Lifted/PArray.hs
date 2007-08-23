module Data.Array.Parallel.Lifted.PArray (
  PArray,

  PA(..),
  emptyPA,

  PRepr, PR(..)
) where

import Data.Array.Parallel.Unlifted ( UArr, emptyU, replicateU )
import GHC.Exts (Int#, Int(..))

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
            , fromPRepr   :: PRepr a -> a
            , dictPRepr   :: PR (PRepr a)
            }

emptyPA :: PA a -> PArray a
emptyPA pa = replicatePA pa 0# (error "PArray.emptyPA: empty")

data PR a = PR {
              lengthPR    :: PArray a -> Int#
            , emptyPR     :: PArray a
            , replicatePR :: Int# -> a -> PArray a
            }




