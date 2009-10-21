{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.PArray (
  PArray(..), PData,

  PA(..),
  lengthPA#, dataPA#, replicatePA#, replicatelPA#, repeatPA#, repeatcPA#,
  emptyPA, indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packPA#, packByTagPA#, combine2PA#, fromListPA#, fromListPA, nfPA,

  replicatePD, replicatelPD, repeatPD, repeatcPD, emptyPD,
  indexPD, extractPD, bpermutePD, appPD, applPD,
  packPD, packByTagPD, combine2PD, fromListPD, fromListPD, nfPD,

  PRepr, PR(..), -- mkPR, mkReprPA,

  T_replicatePR, T_replicatelPR, T_repeatPR, T_repeatcPR, T_emptyPR,
  T_indexPR, T_extractPR, T_bpermutePR, T_appPR, T_applPR,
  T_packPR, T_packByTagPR, T_combine2PR, T_fromListPR, T_fromListPR, T_nfPR
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.Selector
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )
import GHC.Exts (Int#, Int(..), (+#), (*#))

-- |Lifted parallel arrays
--
data PArray a = PArray Int# (PData a)
data family PData a

-- |Representation types
--
type family PRepr a

-- |Dictionaries
--

class PR (PRepr a) => PA a where
  toPRepr      :: a -> PRepr a
  fromPRepr    :: PRepr a -> a
  toArrPRepr   :: PData a -> PData (PRepr a)
  fromArrPRepr :: PData (PRepr a) -> PData a

type T_emptyPR      a =  PData a

type T_replicatePR  a =  Int# -> a -> PData a

type T_replicatelPR a =  U.Segd            -- segd of result array
                      -> PData a -> PData a

type T_repeatPR     a =  Int#              -- number of times to repeat
                      -> Int#              -- length of src array
                      -> PData a -> PData a

type T_repeatcPR    a =  Int#              -- length of result array
                      -> U.Array Int       -- number of times each segment
                                           -- is repeated
                      -> U.Segd            -- src segd
                      -> PData a -> PData a

type T_indexPR      a =  PData a -> Int# -> a

type T_extractPR    a =  PData a
                      -> Int#              -- starting index
                      -> Int#              -- length of result array
                      -> PData a

type T_bpermutePR   a =  PData a
                      -> Int#              -- result length
                      -> U.Array Int       -- indices
                      -> PData a

type T_appPR        a = PData a -> PData a -> PData a

type T_applPR       a =  U.Segd -> PData a   -- src segd/data 1
                      -> U.Segd -> PData a   -- src segd/data 2
                      -> PData a

type T_packPR       a =  PData a
                      -> Int#              -- result length
                      -> U.Array Bool      -- flags
                      -> PData a

type T_packByTagPR  a = PData a
                      -> Int#              -- result length
                      -> U.Array Int       -- tags
                      -> Int#              -- tag value
                      -> PData a

type T_combine2PR   a =  Int#              -- result length
                      -> Sel2              -- selector
                      -> PData a -> PData a -> PData a

type T_fromListPR a = Int# -> [a] -> PData a

type T_nfPR a = PData a -> ()

class PR a where
  emptyPR      :: T_emptyPR a
  replicatePR  :: T_replicatePR a
  replicatelPR :: T_replicatelPR a
  repeatPR     :: T_repeatPR a
  repeatcPR    :: T_repeatcPR a
  indexPR      :: T_indexPR a
  extractPR    :: T_extractPR a
  bpermutePR   :: T_bpermutePR a
  appPR        :: T_appPR a
  applPR       :: T_applPR a
  packPR       :: T_packPR a
  packByTagPR  :: T_packByTagPR a
  combine2PR   :: T_combine2PR a
  fromListPR   :: T_fromListPR a
  nfPR         :: T_nfPR a

emptyPD :: PA a => T_emptyPR a
{-# INLINE_PA emptyPD #-}
emptyPD = fromArrPRepr emptyPR

replicatePD :: PA a => T_replicatePR a
{-# INLINE_PA replicatePD #-}
replicatePD n# x = fromArrPRepr
                 . replicatePR n#
                 $ toPRepr x

replicatelPD :: PA a => T_replicatelPR a
{-# INLINE_PA replicatelPD #-}
replicatelPD segd xs = fromArrPRepr
                     . replicatelPR segd
                     $ toArrPRepr xs
    
repeatPD :: PA a => T_repeatPR a
{-# INLINE_PA repeatPD #-}
repeatPD n# len# xs = fromArrPRepr
                    . repeatPR n# len#
                    $ toArrPRepr xs

repeatcPD :: PA a => T_repeatcPR a
{-# INLINE_PA repeatcPD #-}
repeatcPD n# ns segd xs = fromArrPRepr
                        . repeatcPR n# ns segd
                        $ toArrPRepr xs

indexPD :: PA a => T_indexPR a
{-# INLINE_PA indexPD #-}
indexPD xs i# = fromPRepr $ indexPR (toArrPRepr xs) i#

extractPD :: PA a => T_extractPR a
{-# INLINE_PA extractPD #-}
extractPD xs i# m# = fromArrPRepr $ extractPR (toArrPRepr xs) i# m#

bpermutePD :: PA a => T_bpermutePR a
{-# INLINE bpermutePD #-}
bpermutePD xs n# is = fromArrPRepr $ bpermutePR (toArrPRepr xs) n# is

appPD :: PA a => T_appPR a
{-# INLINE_PA appPD #-}
appPD xs ys = fromArrPRepr $ appPR (toArrPRepr xs) (toArrPRepr ys)

applPD :: PA a => T_applPR a
{-# INLINE_PA applPD #-}
applPD is xs js ys = fromArrPRepr $ applPR is (toArrPRepr xs) js (toArrPRepr ys)

packPD :: PA a => T_packPR a
{-# INLINE_PA packPD #-}
packPD xs n# bs = fromArrPRepr $ packPR (toArrPRepr xs) n# bs

packByTagPD :: PA a => T_packByTagPR a
{-# INLINE_PA packByTagPD #-}
packByTagPD xs n# tags t#
  = fromArrPRepr $ packByTagPR (toArrPRepr xs) n# tags t#

combine2PD :: PA a => T_combine2PR a
{-# INLINE_PA combine2PD #-}
combine2PD n# sel as bs
  = fromArrPRepr $ combine2PR n# sel (toArrPRepr as) (toArrPRepr bs)

fromListPD :: PA a => T_fromListPR a
{-# INLINE_PA fromListPD #-}
fromListPD n# xs = fromArrPRepr $ fromListPR n# (map toPRepr xs)

nfPD :: PA a => T_nfPR a
{-# INLINE nfPD #-}
nfPD xs = nfPR (toArrPRepr xs)


lengthPA# :: PArray a -> Int#
{-# INLINE_PA lengthPA# #-}
lengthPA# (PArray n# _) = n#

dataPA# :: PArray a -> PData a
{-# INLINE_PA dataPA# #-}
dataPA# (PArray _ d) = d

emptyPA :: PA a => PArray a
{-# INLINE_PA emptyPA #-}
emptyPA = PArray 0# emptyPD

replicatePA# :: PA a => Int# -> a -> PArray a
{-# INLINE_PA replicatePA# #-}
replicatePA# n# x = PArray n# (replicatePD n# x)

replicatelPA# :: PA a => U.Segd -> PArray a -> PArray a
{-# INLINE_PA replicatelPA# #-}
replicatelPA# segd (PArray n# xs)
  = PArray (elementsSegd# segd) (replicatelPD segd xs)

repeatPA# :: PA a => Int# -> PArray a -> PArray a
{-# INLINE_PA repeatPA# #-}
repeatPA# m# (PArray n# xs) = PArray (m# *# n#) (repeatPD m# n# xs)

repeatcPA# :: PA a => U.Array Int -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA repeatcPA# #-}
repeatcPA# ns segd (PArray n# xs)
  = case U.sum (U.zipWith (*) ns (U.lengthsSegd segd)) of
      I# m# -> PArray m# (repeatcPD m# ns segd xs)

indexPA# :: PA a => PArray a -> Int# -> a
{-# INLINE_PA indexPA# #-}
indexPA# (PArray _ xs) i# = indexPD xs i#

extractPA# :: PA a => PArray a -> Int# -> Int# -> PArray a
{-# INLINE_PA extractPA# #-}
extractPA# (PArray _ xs) i# n# = PArray n# (extractPD xs i# n#)

bpermutePA# :: PA a => PArray a -> Int# -> U.Array Int -> PArray a
{-# INLINE bpermutePA# #-}
bpermutePA# (PArray _ xs) n# is = PArray n# (bpermutePD xs n# is)

appPA# :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA# #-}
appPA# (PArray m# xs) (PArray n# ys) = PArray (m# +# n#) (appPD xs ys)

applPA# :: PA a => U.Segd -> PArray a -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA applPA# #-}
applPA# is (PArray m# xs) js (PArray n# ys)
  = PArray (m# +# n#) (applPD is xs js ys)

packPA# :: PA a => PArray a -> Int# -> U.Array Bool -> PArray a
{-# INLINE_PA packPA# #-}
packPA# (PArray _ xs) n# bs = PArray n# (packPD xs n# bs)

packByTagPA# :: PA a => PArray a -> Int# -> U.Array Int -> Int# -> PArray a
{-# INLINE_PA packByTagPA# #-}
packByTagPA# (PArray _ xs) n# tags t# = PArray n# (packByTagPD xs n# tags t#)

combine2PA# :: PA a => Int# -> Sel2 -> PArray a -> PArray a -> PArray a
{-# INLINE_PA combine2PA# #-}
combine2PA# n# sel (PArray _ as) (PArray _ bs)
  = PArray n# (combine2PD n# sel as bs)

fromListPA# :: PA a => Int# -> [a] -> PArray a
{-# INLINE_PA fromListPA# #-}
fromListPA# n# xs = PArray n# (fromListPD n# xs)

fromListPA :: PA a => [a] -> PArray a
{-# INLINE fromListPA #-}
fromListPA xs = case length xs of
                  I# n# -> fromListPA# n# xs

nfPA :: PA a => PArray a -> ()
{-# INLINE nfPA #-}
nfPA (PArray _ xs) = nfPD xs

{-
mkPR :: PA a => PR a
{-# INLINE mkPR #-}
mkPR pa = PR {
            emptyPR      = emptyPD pa
          , replicatePR  = replicatePD pa
          , replicatelPR = replicatelPD pa
          , repeatPR     = repeatPD pa
          , repeatcPR    = repeatcPD pa
          , indexPR      = indexPD pa
          , bpermutePR   = bpermutePD pa
          , appPR        = appPD pa
          , applPR       = applPD pa
          , packPR       = packPD pa
          , combine2PR   = combine2PD pa
          , fromListPR   = fromListPD pa
          , nfPR         = nfPD pa
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
-}

