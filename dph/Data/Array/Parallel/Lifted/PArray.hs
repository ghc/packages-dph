{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.PArray (
  PArray,

  PA(..),
  lengthPA#, replicatePA#, replicatelPA#, repeatPA#, emptyPA,
  indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packPA#, combine2PA#, fromListPA#, fromListPA, nfPA,

  PRepr, PR(..), mkPR, mkReprPA
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.Unboxed ( Segd, PArray_Int#, PArray_Bool# )
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
              toPRepr      :: a                -> PRepr a
            , fromPRepr    :: PRepr a          -> a
            , toArrPRepr   :: PArray a         -> PArray (PRepr a)
            , fromArrPRepr :: PArray (PRepr a) -> PArray a
            , dictPRepr    :: PR (PRepr a)
            }

lengthPA# :: PA a -> PArray a -> Int#
{-# INLINE_PA lengthPA# #-}
lengthPA# pa x = lengthPR (dictPRepr pa) (toArrPRepr pa x)

emptyPA :: PA a -> PArray a
{-# INLINE_PA emptyPA #-}
emptyPA pa = fromArrPRepr pa
           $ emptyPR (dictPRepr pa)

replicatePA# :: PA a -> Int# -> a -> PArray a
{-# INLINE_PA replicatePA# #-}
replicatePA# pa n# = fromArrPRepr pa
                   . replicatePR (dictPRepr pa) n#
                   . toPRepr pa

replicatelPA# :: PA a -> Int# -> PArray_Int# -> PArray a -> PArray a
{-# INLINE_PA replicatelPA# #-}
replicatelPA# pa n# ns = fromArrPRepr pa
                       . replicatelPR (dictPRepr pa) n# ns
                       . toArrPRepr pa

repeatPA# :: PA a -> Int# -> PArray a -> PArray a
{-# INLINE_PA repeatPA# #-}
repeatPA# pa n# = fromArrPRepr pa
                . repeatPR (dictPRepr pa) n#
                . toArrPRepr pa


indexPA# :: PA a -> PArray a -> Int# -> a
{-# INLINE_PA indexPA# #-}
indexPA# pa xs i# = fromPRepr pa
                  $ indexPR (dictPRepr pa) (toArrPRepr pa xs) i#

extractPA# :: PA a -> PArray a -> Int# -> Int# -> PArray a
{-# INLINE_PA extractPA# #-}
extractPA# pa xs i# n# = fromArrPRepr pa
                       $ extractPR (dictPRepr pa) (toArrPRepr pa xs) i# n#

bpermutePA# :: PA a -> Int# -> PArray a -> PArray_Int# -> PArray a
{-# INLINE bpermutePA# #-}
bpermutePA# pa n# xs is = fromArrPRepr pa
                        $ bpermutePR (dictPRepr pa) n# (toArrPRepr pa xs) is

appPA# :: PA a -> PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA# #-}
appPA# pa xs ys = fromArrPRepr pa
                $ appPR (dictPRepr pa) (toArrPRepr pa xs) (toArrPRepr pa ys)

applPA# :: PA a -> Segd -> PArray a -> Segd -> PArray a -> PArray a
{-# INLINE_PA applPA# #-}
applPA# pa is xs js ys = fromArrPRepr pa
                       $ applPR (dictPRepr pa) is (toArrPRepr pa xs)
                                               js (toArrPRepr pa ys)

packPA# :: PA a -> PArray a -> Int# -> PArray_Bool# -> PArray a
{-# INLINE_PA packPA# #-}
packPA# pa arr n# = fromArrPRepr pa
                  . packPR (dictPRepr pa) (toArrPRepr pa arr) n#

combine2PA# :: PA a -> Int# -> PArray_Int# -> PArray_Int#
            -> PArray a -> PArray a -> PArray a
{-# INLINE_PA combine2PA# #-}
combine2PA# pa n# sel# is# as bs
  = fromArrPRepr pa
  $ combine2PR (dictPRepr pa) n# sel# is# (toArrPRepr pa as) (toArrPRepr pa bs)

fromListPA# :: PA a -> Int# -> [a] -> PArray a
{-# INLINE_PA fromListPA# #-}
fromListPA# pa n# xs = fromArrPRepr pa
                     $ fromListPR (dictPRepr pa) n# (map (toPRepr pa) xs)

fromListPA :: PA a -> [a] -> PArray a
{-# INLINE fromListPA #-}
fromListPA pa xs = case length xs of
                     I# n# -> fromListPA# pa n# xs

nfPA :: PA a -> PArray a -> ()
{-# INLINE nfPA #-}
nfPA pa xs = nfPR (dictPRepr pa) $ toArrPRepr pa xs

data PR a = PR {
              lengthPR     :: PArray a -> Int#
            , emptyPR      :: PArray a
            , replicatePR  :: Int# -> a -> PArray a
            , replicatelPR :: Int# -> PArray_Int# -> PArray a -> PArray a
            , repeatPR     :: Int# -> PArray a -> PArray a
            , indexPR      :: PArray a -> Int# -> a
            , extractPR    :: PArray a -> Int# -> Int# -> PArray a
            , bpermutePR   :: Int# -> PArray a -> PArray_Int# -> PArray a
            , appPR        :: PArray a -> PArray a -> PArray a
            , applPR       :: Segd -> PArray a -> Segd -> PArray a -> PArray a
            , packPR       :: PArray a -> Int# -> PArray_Bool# -> PArray a
            , combine2PR   :: Int# -> PArray_Int# -> PArray_Int#
                              -> PArray a -> PArray a -> PArray a
            , fromListPR   :: Int# -> [a] -> PArray a
            , nfPR         :: PArray a -> ()
            }

mkPR :: PA a -> PR a
{-# INLINE mkPR #-}
mkPR pa = PR {
            lengthPR     = lengthPA# pa
          , emptyPR      = emptyPA pa
          , replicatePR  = replicatePA# pa
          , replicatelPR = replicatelPA# pa
          , repeatPR     = repeatPA# pa
          , indexPR      = indexPA# pa
          , bpermutePR   = bpermutePA# pa
          , appPR        = appPA# pa
          , applPR       = applPA# pa
          , packPR       = packPA# pa
          , combine2PR   = combine2PA# pa
          , fromListPR   = fromListPA# pa
          , nfPR         = nfPA pa
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

