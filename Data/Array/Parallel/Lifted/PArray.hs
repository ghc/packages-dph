module Data.Array.Parallel.Lifted.PArray (
  PArray,

  PA(..),
  lengthPA, replicatePA, emptyPA, packPA,

  PRepr, PR(..), mkPR, mkReprPA
) where

import Data.Array.Parallel.Unlifted ( UArr )
import Data.Array.Parallel.Lifted.Prim ( PArray_Bool# )
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
              toPRepr      :: a                -> PRepr a
            , fromPRepr    :: PRepr a          -> a
            , toArrPRepr   :: PArray a         -> PArray (PRepr a)
            , fromArrPRepr :: PArray (PRepr a) -> PArray a
            , dictPRepr    :: PR (PRepr a)
            }

lengthPA :: PA a -> PArray a -> Int#
{-# INLINE lengthPA #-}
lengthPA pa x = lengthPR (dictPRepr pa) (toArrPRepr pa x)

replicatePA :: PA a -> Int# -> a -> PArray a
{-# INLINE replicatePA #-}
replicatePA pa n# = fromArrPRepr pa
                  . replicatePR (dictPRepr pa) n#
                  . toPRepr pa

emptyPA :: PA a -> PArray a
{-# INLINE emptyPA #-}
emptyPA pa = fromArrPRepr pa
           $ emptyPR (dictPRepr pa)

packPA :: PA a -> PArray a -> Int# -> PArray_Bool# -> PArray a
{-# INLINE packPA #-}
packPA pa arr n# = fromArrPRepr pa
                 . packPR (dictPRepr pa) (toArrPRepr pa arr) n#

data PR a = PR {
              lengthPR    :: PArray a -> Int#
            , emptyPR     :: PArray a
            , replicatePR :: Int# -> a -> PArray a
            , packPR      :: PArray a -> Int# -> PArray_Bool# -> PArray a
            }

mkPR :: PA a -> PR a
{-# INLINE mkPR #-}
mkPR pa = PR {
            lengthPR    = lengthPA pa
          , emptyPR     = emptyPA pa
          , replicatePR = replicatePA pa
          , packPR      = packPA pa
          }

mkReprPA :: (a ~ PRepr a) => PR a -> PA a
{-# INLINE mkReprPA #-}
mkReprPA pr = PA {
                toPRepr      = id
              , fromPRepr    = id
              , toArrPRepr   = id
              , fromArrPRepr = id
              , dictPRepr    = pr
              }

