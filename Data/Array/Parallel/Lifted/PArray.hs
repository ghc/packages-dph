module Data.Array.Parallel.Lifted.PArray (
  PArray,

  PA(..),
  lengthPA#, replicatePA#, replicatelPA#, emptyPA, indexPA#,
  packPA#, combine2PA#,

  PRepr, PR(..), mkPR, mkReprPA
) where

import Data.Array.Parallel.Unlifted ( UArr )
import Data.Array.Parallel.Lifted.Prim ( PArray_Int#, PArray_Bool# )
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

lengthPA# :: PA a -> PArray a -> Int#
{-# INLINE lengthPA# #-}
lengthPA# pa x = lengthPR (dictPRepr pa) (toArrPRepr pa x)

replicatePA# :: PA a -> Int# -> a -> PArray a
{-# INLINE replicatePA# #-}
replicatePA# pa n# = fromArrPRepr pa
                   . replicatePR (dictPRepr pa) n#
                   . toPRepr pa

replicatelPA# :: PA a -> Int# -> PArray_Int# -> PArray a -> PArray a
{-# INLINE replicatelPA# #-}
replicatelPA# pa n# ns = fromArrPRepr pa
                       . replicatelPR (dictPRepr pa) n# ns
                       . toArrPRepr pa

emptyPA :: PA a -> PArray a
{-# INLINE emptyPA #-}
emptyPA pa = fromArrPRepr pa
           $ emptyPR (dictPRepr pa)

indexPA# :: PA a -> PArray a -> Int# -> a
{-# INLINE indexPA# #-}
indexPA# pa xs i# = fromPRepr pa
                  $ indexPR (dictPRepr pa) (toArrPRepr pa xs) i#

packPA# :: PA a -> PArray a -> Int# -> PArray_Bool# -> PArray a
{-# INLINE packPA# #-}
packPA# pa arr n# = fromArrPRepr pa
                  . packPR (dictPRepr pa) (toArrPRepr pa arr) n#

combine2PA# :: PA a -> Int# -> PArray_Int# -> PArray_Int#
            -> PArray a -> PArray a -> PArray a
{-# INLINE combine2PA# #-}
combine2PA# pa n# sel# is# as bs
  = fromArrPRepr pa
  $ combine2PR (dictPRepr pa) n# sel# is# (toArrPRepr pa as) (toArrPRepr pa bs)

data PR a = PR {
              lengthPR     :: PArray a -> Int#
            , emptyPR      :: PArray a
            , replicatePR  :: Int# -> a -> PArray a
            , replicatelPR :: Int# -> PArray_Int# -> PArray a -> PArray a
            , indexPR      :: PArray a -> Int# -> a
            , packPR       :: PArray a -> Int# -> PArray_Bool# -> PArray a
            , combine2PR   :: Int# -> PArray_Int# -> PArray_Int#
                              -> PArray a -> PArray a -> PArray a
            }

mkPR :: PA a -> PR a
{-# INLINE mkPR #-}
mkPR pa = PR {
            lengthPR     = lengthPA# pa
          , emptyPR      = emptyPA pa
          , replicatePR  = replicatePA# pa
          , replicatelPR = replicatelPA# pa
          , indexPR      = indexPA# pa
          , packPR       = packPA# pa
          , combine2PR   = combine2PA# pa
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

