{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.PArray (
  PArray(..), PData,

  PA(..),
  lengthPA#, dataPA#, replicatePA#, replicatelPA#, repeatPA#, repeatcPA#,
  emptyPA, indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packPA#, combine2PA#, fromListPA#, fromListPA, nfPA,

  replicatePD, replicatelPD, repeatPD, repeatcPD, emptyPD,
  indexPD, extractPD, bpermutePD, appPD, applPD,
  packPD, combine2PD, fromListPD, fromListPD, nfPD,

  PRepr, PR(..), mkPR, mkReprPA,

  T_replicatePR, T_replicatelPR, T_repeatPR, T_repeatcPR, T_emptyPR,
  T_indexPR, T_extractPR, T_bpermutePR, T_appPR, T_applPR,
  T_packPR, T_combine2PR, T_fromListPR, T_fromListPR, T_nfPR
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

data PA a = PA {
              toPRepr      :: a               -> PRepr a
            , fromPRepr    :: PRepr a         -> a
            , toArrPRepr   :: PData a         -> PData (PRepr a)
            , fromArrPRepr :: PData (PRepr a) -> PData a
            , dictPRepr    :: PR (PRepr a)
            }


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

type T_combine2PR   a =  Int#              -- result length
                      -> Sel2              -- selector
                      -> PData a -> PData a -> PData a

type T_fromListPR a = Int# -> [a] -> PData a

type T_nfPR a = PData a -> ()

data PR a = PR {
              emptyPR      :: T_emptyPR a
            , replicatePR  :: T_replicatePR a
            , replicatelPR :: T_replicatelPR a
            , repeatPR     :: T_repeatPR a
            , repeatcPR    :: T_repeatcPR a
            , indexPR      :: T_indexPR a
            , extractPR    :: T_extractPR a
            , bpermutePR   :: T_bpermutePR a
            , appPR        :: T_appPR a
            , applPR       :: T_applPR a
            , packPR       :: T_packPR a
            , combine2PR   :: T_combine2PR a
            , fromListPR   :: T_fromListPR a
            , nfPR         :: T_nfPR a
            }

emptyPD :: PA a -> T_emptyPR a
{-# INLINE_PA emptyPD #-}
emptyPD pa = fromArrPRepr pa
          $ emptyPR (dictPRepr pa)

replicatePD :: PA a -> T_replicatePR a
{-# INLINE_PA replicatePD #-}
replicatePD pa n# x = fromArrPRepr pa
                    . replicatePR (dictPRepr pa) n#
                    $ toPRepr pa x

replicatelPD :: PA a -> T_replicatelPR a
{-# INLINE_PA replicatelPD #-}
replicatelPD pa segd xs = fromArrPRepr pa
                        . replicatelPR (dictPRepr pa) segd
                        $ toArrPRepr pa xs
    
repeatPD :: PA a -> T_repeatPR a
{-# INLINE_PA repeatPD #-}
repeatPD pa n# len# xs = fromArrPRepr pa
                       . repeatPR (dictPRepr pa) n# len#
                       $ toArrPRepr pa xs

repeatcPD :: PA a -> T_repeatcPR a
{-# INLINE_PA repeatcPD #-}
repeatcPD pa n# ns segd xs = fromArrPRepr pa
                           . repeatcPR (dictPRepr pa) n# ns segd
                           $ toArrPRepr pa xs

indexPD :: PA a -> T_indexPR a
{-# INLINE_PA indexPD #-}
indexPD pa xs i# = fromPRepr pa
                 $ indexPR (dictPRepr pa) (toArrPRepr pa xs) i#

extractPD :: PA a -> T_extractPR a
{-# INLINE_PA extractPD #-}
extractPD pa xs i# m# = fromArrPRepr pa
                      $ extractPR (dictPRepr pa) (toArrPRepr pa xs) i# m#

bpermutePD :: PA a -> T_bpermutePR a
{-# INLINE bpermutePD #-}
bpermutePD pa xs n# is = fromArrPRepr pa
                       $ bpermutePR (dictPRepr pa) (toArrPRepr pa xs) n# is

appPD :: PA a -> T_appPR a
{-# INLINE_PA appPD #-}
appPD pa xs ys = fromArrPRepr pa
               $ appPR (dictPRepr pa) (toArrPRepr pa xs) (toArrPRepr pa ys)

applPD :: PA a -> T_applPR a
{-# INLINE_PA applPD #-}
applPD pa is xs js ys = fromArrPRepr pa
                      $ applPR (dictPRepr pa) is (toArrPRepr pa xs)
                                              js (toArrPRepr pa ys)

packPD :: PA a -> T_packPR a
{-# INLINE_PA packPD #-}
packPD pa xs n# bs = fromArrPRepr pa
                   $ packPR (dictPRepr pa) (toArrPRepr pa xs) n# bs

combine2PD :: PA a -> T_combine2PR a
{-# INLINE_PA combine2PD #-}
combine2PD pa n# sel as bs
  = fromArrPRepr pa
  $ combine2PR (dictPRepr pa) n# sel (toArrPRepr pa as)
                                     (toArrPRepr pa bs)

fromListPD :: PA a -> T_fromListPR a
{-# INLINE_PA fromListPD #-}
fromListPD pa n# xs = fromArrPRepr pa
                    $ fromListPR (dictPRepr pa) n# (map (toPRepr pa) xs)

nfPD :: PA a -> T_nfPR a
{-# INLINE nfPD #-}
nfPD pa xs = nfPR (dictPRepr pa) (toArrPRepr pa xs)




lengthPA# :: PArray a -> Int#
{-# INLINE_PA lengthPA# #-}
lengthPA# (PArray n# _) = n#

dataPA# :: PArray a -> PData a
{-# INLINE_PA dataPA# #-}
dataPA# (PArray _ d) = d

emptyPA :: PA a -> PArray a
{-# INLINE_PA emptyPA #-}
emptyPA pa = PArray 0# (emptyPD pa)

replicatePA# :: PA a -> Int# -> a -> PArray a
{-# INLINE_PA replicatePA# #-}
replicatePA# pa n# x = PArray n# (replicatePD pa n# x)

replicatelPA# :: PA a -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA replicatelPA# #-}
replicatelPA# pa segd (PArray n# xs)
  = PArray (elementsSegd# segd) (replicatelPD pa segd xs)

repeatPA# :: PA a -> Int# -> PArray a -> PArray a
{-# INLINE_PA repeatPA# #-}
repeatPA# pa m# (PArray n# xs) = PArray (m# *# n#) (repeatPD pa m# n# xs)

repeatcPA# :: PA a -> U.Array Int -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA repeatcPA# #-}
repeatcPA# pa ns segd (PArray n# xs)
  = case U.sum (U.zipWith (*) ns (U.lengthsSegd segd)) of
      I# m# -> PArray m# (repeatcPD pa m# ns segd xs)

indexPA# :: PA a -> PArray a -> Int# -> a
{-# INLINE_PA indexPA# #-}
indexPA# pa (PArray _ xs) i# = indexPD pa xs i#

extractPA# :: PA a -> PArray a -> Int# -> Int# -> PArray a
{-# INLINE_PA extractPA# #-}
extractPA# pa (PArray _ xs) i# n# = PArray n# (extractPD pa xs i# n#)

bpermutePA# :: PA a -> PArray a -> Int# -> U.Array Int -> PArray a
{-# INLINE bpermutePA# #-}
bpermutePA# pa (PArray _ xs) n# is = PArray n# (bpermutePD pa xs n# is)

appPA# :: PA a -> PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA# #-}
appPA# pa (PArray m# xs) (PArray n# ys) = PArray (m# +# n#) (appPD pa xs ys)

applPA# :: PA a -> U.Segd -> PArray a -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA applPA# #-}
applPA# pa is (PArray m# xs) js (PArray n# ys)
  = PArray (m# +# n#) (applPD pa is xs js ys)

packPA# :: PA a -> PArray a -> Int# -> U.Array Bool -> PArray a
{-# INLINE_PA packPA# #-}
packPA# pa (PArray _ xs) n# bs = PArray n# (packPD pa xs n# bs)

combine2PA# :: PA a -> Int# -> Sel2 -> PArray a -> PArray a -> PArray a
{-# INLINE_PA combine2PA# #-}
combine2PA# pa n# sel (PArray _ as) (PArray _ bs)
  = PArray n# (combine2PD pa n# sel as bs)

fromListPA# :: PA a -> Int# -> [a] -> PArray a
{-# INLINE_PA fromListPA# #-}
fromListPA# pa n# xs = PArray n# (fromListPD pa n# xs)

fromListPA :: PA a -> [a] -> PArray a
{-# INLINE fromListPA #-}
fromListPA pa xs = case length xs of
                     I# n# -> fromListPA# pa n# xs

nfPA :: PA a -> PArray a -> ()
{-# INLINE nfPA #-}
nfPA pa (PArray _ xs) = nfPD pa xs

mkPR :: PA a -> PR a
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

