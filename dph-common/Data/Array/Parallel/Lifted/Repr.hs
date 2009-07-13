{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Repr (
  PData(..),
  Void, void, pvoid, dPA_Void, dPR_Void,
{-
  Wrap(..),
  Sum2(..), Sum3(..), 
-}

  punit, dPR_Unit,
  Wrap(..), dPR_Wrap,

  dPR_2, dPR_3, dPR_4, dPR_5, zipPA#, unzipPA#, zip3PA#,
  Sum2(..), {-Sum3(..),-} dPR_Sum2, {-dPR_Sum3,-}

  dPR_PArray, segdPA#, concatPA#, segmentPA#, copySegdPA#
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Selector
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base ((:*:)(..), fromBool)

import qualified Data.List as L
import GHC.Exts  (Int#, Int(..), (+#), (-#), (*#))
import Debug.Trace


data Void

traceFn   = flip const -- trace
traceArgs = flip  const -- trace 

void :: Void
void = error "Data.Array.Parallel.void"

data instance PData Void

pvoid :: PData Void
pvoid = error "Data.Array.Parallel.PData Void"

dPR_Void :: PR Void
{-# INLINE dPR_Void #-}
dPR_Void = PR {
             emptyPR      = emptyPR_Void
           , replicatePR  = replicatePR_Void
           , replicatelPR = replicatelPR_Void
           , repeatPR     = repeatPR_Void
           , repeatcPR    = repeatcPR_Void
           , indexPR      = indexPR_Void
           , extractPR    = extractPR_Void
           , bpermutePR   = bpermutePR_Void
           , appPR        = appPR_Void
           , applPR       = applPR_Void
           , packPR       = packPR_Void
           , combine2PR   = combine2PR_Void
           , fromListPR   = fromListPR_Void
           , nfPR         = nfPR_Void
           }

emptyPR_Void :: T_emptyPR Void
{-# INLINE emptyPR_Void #-}
emptyPR_Void = traceFn "emptyPR_Void" $ pvoid

replicatePR_Void :: T_replicatePR Void
{-# INLINE replicatePR_Void #-}
replicatePR_Void _ _ = traceFn "replicatePR_Void" $ pvoid

replicatelPR_Void :: T_replicatelPR Void
{-# INLINE replicatelPR_Void #-}
replicatelPR_Void segd _ = traceFn "replicatelPR_Void" $ pvoid

repeatPR_Void :: T_repeatPR Void
{-# INLINE repeatPR_Void #-}
repeatPR_Void _ _ _ = traceFn "repeatPR_Void" $ pvoid

repeatcPR_Void :: T_repeatcPR Void
{-# INLINE repeatcPR_Void #-}
repeatcPR_Void _ _ _ _ = traceFn "repeatcPR_Void" $ pvoid

indexPR_Void :: T_indexPR Void
{-# INLINE indexPR_Void #-}
indexPR_Void _ _ = traceFn "indexPR_Void" $ void

extractPR_Void :: T_extractPR Void
{-# INLINE extractPR_Void #-}
extractPR_Void  _ _ _ = traceFn "extractPR_Void" $ pvoid

bpermutePR_Void :: T_bpermutePR Void
{-# INLINE bpermutePR_Void #-}
bpermutePR_Void _ _ _ = traceFn "bpermutePR_Void" $ pvoid

appPR_Void :: T_appPR Void
{-# INLINE appPR_Void #-}
appPR_Void  _ _ = traceFn "appPR_Void" $ pvoid

applPR_Void :: T_applPR Void
{-# INLINE applPR_Void #-}
applPR_Void _ _ _ _ = traceFn "applPR_Void" $ pvoid

packPR_Void :: T_packPR Void
{-# INLINE packPR_Void #-}
packPR_Void _ _ _ = traceFn "packPR_Void" $ pvoid

combine2PR_Void :: T_combine2PR Void
{-# INLINE combine2PR_Void #-}
combine2PR_Void _ _ _ _ = traceFn "combine2PR_Void" $ pvoid

fromListPR_Void :: T_fromListPR Void
{-# INLINE fromListPR_Void #-}
fromListPR_Void _ _ = pvoid

nfPR_Void :: T_nfPR Void
{-# INLINE nfPR_Void #-}
nfPR_Void _ = ()

type instance PRepr Void = Void

dPA_Void :: PA Void
{-# INLINE_PA dPA_Void #-}
dPA_Void = PA {
             toPRepr      = id
           , fromPRepr    = id
           , toArrPRepr   = id
           , fromArrPRepr = id
           , dictPRepr    = dPR_Void
           }



data instance PData () = PUnit

punit :: PData ()
punit = PUnit

dPR_Unit :: PR ()
{-# INLINE dPR_Unit #-}
dPR_Unit = PR {
             emptyPR      = emptyPR_Unit
           , replicatePR  = replicatePR_Unit
           , replicatelPR = replicatelPR_Unit
           , repeatPR     = repeatPR_Unit
           , repeatcPR    = repeatcPR_Unit
           , indexPR      = indexPR_Unit
           , extractPR    = extractPR_Unit
           , bpermutePR   = bpermutePR_Unit
           , appPR        = appPR_Unit
           , applPR       = applPR_Unit
           , packPR       = packPR_Unit
           , combine2PR   = combine2PR_Unit
           , fromListPR   = fromListPR_Unit
           , nfPR         = nfPR_Unit
           }

emptyPR_Unit :: T_emptyPR ()         
{-# INLINE emptyPR_Unit #-}
emptyPR_Unit = traceFn "emptyPR_Unit" $ PUnit

replicatePR_Unit :: T_replicatePR ()
{-# INLINE replicatePR_Unit #-}
replicatePR_Unit n# () = 
  traceFn "replicatePR_Unit" $
  traceArgs ("replicatePR_Unit len = " ++ show (I# n#)) $
  PUnit

replicatelPR_Unit :: T_replicatelPR ()
{-# INLINE replicatelPR_Unit #-}
replicatelPR_Unit segd u =
  traceFn "replicatelPR_Unit" 
  traceArgs ("replicatelPR_Unit args len = " ++ (show (U.elementsSegd segd))) $
  u

repeatPR_Unit :: T_repeatPR ()
{-# INLINE repeatPR_Unit #-}
repeatPR_Unit _ _ u = traceFn "repeatPR_Unit" $ u

repeatcPR_Unit :: T_repeatcPR ()
{-# INLINE repeatcPR_Unit #-}
repeatcPR_Unit _ _ _ u = traceFn "repeatcPR_Unit" $ u

indexPR_Unit :: T_indexPR ()
{-# INLINE indexPR_Unit #-}
indexPR_Unit PUnit _ = traceFn "indexPR_Unit" $ ()

extractPR_Unit :: T_extractPR ()
{-# INLINE extractPR_Unit #-}
extractPR_Unit u _ _ = traceFn "extractPR_Unit" $ u

bpermutePR_Unit :: T_bpermutePR ()
{-# INLINE bpermutePR_Unit #-}
bpermutePR_Unit u _ _ = traceFn "bpermutePR_Unit" $ u

appPR_Unit :: T_appPR ()
{-# INLINE appPR_Unit #-}
appPR_Unit u v = traceFn "appPR_Unit" (u `seq` v)

applPR_Unit :: T_applPR ()
{-# INLINE applPR_Unit #-}
applPR_Unit _ u _ v = traceFn "applPR_Unit" (u `seq` v)

packPR_Unit :: T_packPR ()
{-# INLINE packPR_Unit #-}
packPR_Unit u _ _ = traceFn "packPR_Unit" $ u

combine2PR_Unit :: T_combine2PR ()
{-# INLINE combine2PR_Unit #-}
combine2PR_Unit _ _ u v = traceFn "combine2PR_Unit" (u `seq` v)

fromListPR_Unit :: T_fromListPR ()
{-# INLINE fromListPR_Unit #-}
fromListPR_Unit _ xs = foldr seq PUnit xs

{-# INLINE nfPR_Unit #-}
nfPR_Unit PUnit = ()

data Wrap a = Wrap a

data instance PData (Wrap a) = PWrap (PData a)

dPR_Wrap :: PR a -> PR (Wrap a)
{-# INLINE dPR_Wrap #-}
dPR_Wrap pr = PR {
              emptyPR      = emptyPR_Wrap pr
            , replicatePR  = replicatePR_Wrap pr
            , replicatelPR = replicatelPR_Wrap pr
            , repeatPR     = repeatPR_Wrap pr
            , repeatcPR    = repeatcPR_Wrap pr
            , indexPR      = indexPR_Wrap pr
            , extractPR    = extractPR_Wrap pr
            , bpermutePR   = bpermutePR_Wrap pr
            , appPR        = appPR_Wrap pr
            , applPR       = applPR_Wrap pr
            , packPR       = packPR_Wrap pr
            }

{-# INLINE emptyPR_Wrap #-}
emptyPR_Wrap pr = traceFn "emptyPR_Wrap" $ PWrap (emptyPR pr)

{-# INLINE replicatePR_Wrap #-}
replicatePR_Wrap pr n# (Wrap x) = traceFn "replicatePR_Wrap" $
                 PWrap (replicatePR pr n# x)

{-# INLINE replicatelPR_Wrap #-}
replicatelPR_Wrap pr segd (PWrap xs) = traceFn "replicatelPR_Wrap" $
                  PWrap (replicatelPR pr segd xs)

{-# INLINE repeatPR_Wrap #-}
repeatPR_Wrap pr n# len# (PWrap xs) = traceFn "repeatPR_Wrap" $
              PWrap (repeatPR pr n# len# xs)

{-# INLINE repeatcPR_Wrap #-}
repeatcPR_Wrap pr n# is segd (PWrap xs) = traceFn "repeatcPR_Wrap" $
              PWrap (repeatcPR pr n# is segd xs)

{-# INLINE indexPR_Wrap #-}
indexPR_Wrap pr (PWrap xs) i# = traceFn "indexPR_Wrap" $
             Wrap (indexPR pr xs i#)

{-# INLINE extractPR_Wrap #-}
extractPR_Wrap pr (PWrap xs) i# n# = traceFn "extractPR_Wrap" $
             PWrap (extractPR pr xs i# n#)

{-# INLINE bpermutePR_Wrap #-}
bpermutePR_Wrap pr (PWrap xs) n# is = traceFn "bpermutePR_Wrap" $
                PWrap (bpermutePR pr xs n# is)

{-# INLINE appPR_Wrap #-}
appPR_Wrap pr (PWrap xs) (PWrap ys) = traceFn "appPR_Wrap" $
           PWrap (appPR pr xs ys)

{-# INLINE applPR_Wrap #-}
applPR_Wrap pr is (PWrap xs) js (PWrap ys) = traceFn "applPR_Wrap" $
            PWrap (applPR pr is xs js ys)

{-# INLINE packPR_Wrap #-}
packPR_Wrap pr (PWrap xs) n# sel# = traceFn "packPR_Wrap" $
            PWrap (packPR pr xs n# sel#)


data instance PData (a,b)
  = P_2 (PData a)
        (PData b)

data instance PData (a,b,c)
  = P_3 (PData a)
        (PData b)
        (PData c)

data instance PData (a,b,c,d)
  = P_4 (PData a)
        (PData b)
        (PData c)
        (PData d)

data instance PData (a,b,c,d,e)
  = P_5 (PData a)
        (PData b)
        (PData c)
        (PData d)
        (PData e)

dPR_2 :: PR a -> PR b -> PR (a,b)
{-# INLINE dPR_2 #-}
dPR_2 pra prb
  = PR {
      emptyPR      = emptyPR_2 pra prb
    , replicatePR  = replicatePR_2 pra prb
    , replicatelPR = replicatelPR_2 pra prb
    , repeatPR     = repeatPR_2 pra prb
    , repeatcPR    = repeatcPR_2 pra prb
    , indexPR      = indexPR_2 pra prb
    , extractPR    = extractPR_2 pra prb
    , bpermutePR   = bpermutePR_2 pra prb
    , appPR        = appPR_2 pra prb
    , applPR       = applPR_2 pra prb
    , packPR       = packPR_2 pra prb
    , combine2PR   = combine2PR_2 pra prb
    , fromListPR   = fromListPR_2 pra prb
    , nfPR         = nfPR_2 pra prb
    }

{-# INLINE emptyPR_2 #-}
emptyPR_2 pra prb = traceFn "emptyPR_2" $
          P_2 (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_2 #-}
replicatePR_2 pra prb n# (a,b) = 
  traceFn "replicatePR_2" $
  traceArgs ("replicatePR_2 args len = "  ++ (show (I# n#))) $
  P_2 (replicatePR pra n# a)
      (replicatePR prb n# b)

{-# INLINE replicatelPR_2 #-}
replicatelPR_2 pra prb segd (P_2 as bs)
  = traceFn "replicatelPR_2" $
  P_2 (replicatelPR pra segd as)
      (replicatelPR prb segd bs) 

{-# INLINE repeatPR_2 #-}
repeatPR_2 pra prb n# len# (P_2 as bs)
  = traceFn "repeatPR_2" $
  P_2 (repeatPR pra n# len# as)
      (repeatPR prb n# len# bs)

{-# INLINE repeatcPR_2 #-}
repeatcPR_2 pra prb n# ns segd (P_2 as bs)
  = traceFn "repeatcPR_2" $
  P_2 (repeatcPR pra n# ns segd as)
      (repeatcPR prb n# ns segd bs)

{-# INLINE indexPR_2 #-}
indexPR_2 pra prb (P_2 as bs) i# = traceFn "indexPR_2" $
          (indexPR pra as i#, indexPR prb bs i#)

{-# INLINE extractPR_2 #-}
extractPR_2 pra prb (P_2 as bs) i# n# = traceFn "extractPR_2" $
          P_2 (extractPR pra as i# n#)
              (extractPR prb bs i# n#)

{-# INLINE bpermutePR_2 #-}
bpermutePR_2 pra prb (P_2 as bs) n# is
  = traceFn "bpermutePR_2" $
  P_2 (bpermutePR pra as n# is) (bpermutePR prb bs n# is)

{-# INLINE appPR_2 #-}
appPR_2 pra prb (P_2 as1 bs1) (P_2 as2 bs2)
  = P_2 (appPR pra as1 as2) (appPR prb bs1 bs2)

{-# INLINE applPR_2 #-}
applPR_2 pra prb is (P_2 as1 bs1) js (P_2 as2 bs2)
  = traceFn "applPR_2" $
  P_2 (applPR pra is as1 js as2)
      (applPR prb is bs1 js bs2)

{-# INLINE packPR_2 #-}
packPR_2 pra prb (P_2 as bs) n# sel# = traceFn "packPR_2" $
         P_2 (packPR pra as n# sel#)
             (packPR prb bs n# sel#)

{-# INLINE combine2PR_2 #-}
combine2PR_2 pra prb n# sel (P_2 as1 bs1) (P_2 as2 bs2)
  = traceFn "combine2PR_2" $
       P_2 (combine2PR pra n# sel as1 as2)
           (combine2PR prb n# sel bs1 bs2)

{-# INLINE fromListPR_2 #-}
fromListPR_2 pra prb n# xs
  = P_2 (fromListPR pra n# as)
        (fromListPR prb n# bs)
  where
    (as,bs) = unzip xs

{-# INLINE nfPR_2 #-}
nfPR_2 pra prb (P_2 as bs)
  = nfPR pra as `seq` nfPR prb bs

zipPA# :: PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA# #-}
zipPA# (PArray n# xs) (PArray _ ys) = PArray n# (P_2 xs ys)

unzipPA# :: PArray (a,b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA# #-}
unzipPA# (PArray n# (P_2 xs ys)) = (PArray n# xs, PArray n# ys)

dPR_3 :: PR a -> PR b -> PR c -> PR (a,b,c)
{-# INLINE dPR_3 #-}
dPR_3 pra prb prc
  = PR {
      emptyPR      = emptyPR_3 pra prb prc
    , replicatePR  = replicatePR_3 pra prb prc
    , replicatelPR = replicatelPR_3 pra prb prc
    , repeatPR     = repeatPR_3 pra prb prc
    , repeatcPR    = repeatcPR_3 pra prb prc
    , indexPR      = indexPR_3 pra prb prc
    , extractPR    = extractPR_3 pra prb prc
    , bpermutePR   = bpermutePR_3 pra prb prc
    , appPR        = appPR_3 pra prb prc
    , applPR       = applPR_3 pra prb prc
    , packPR       = packPR_3 pra prb prc
    , combine2PR   = combine2PR_3 pra prb prc
    , fromListPR   = fromListPR_3 pra prb prc
    , nfPR         = nfPR_3 pra prb prc
    }

{-# INLINE emptyPR_3 #-}
emptyPR_3 pra prb prc = traceFn "emptyPR_3" $
          P_3 (emptyPR pra) (emptyPR prb) (emptyPR prc)

{-# INLINE replicatePR_3 #-}
replicatePR_3 pra prb prc n# (a,b,c)
  = traceFn "replicatePR_3" $
  P_3 (replicatePR pra n# a)
      (replicatePR prb n# b)
      (replicatePR prc n# c)

{-# INLINE replicatelPR_3 #-}
replicatelPR_3 pra prb prc segd (P_3 as bs cs)
  = traceFn "replicatelPR_3" $
  P_3 (replicatelPR pra segd as)
      (replicatelPR prb segd bs)
      (replicatelPR prc segd cs)

{-# INLINE repeatPR_3 #-}
repeatPR_3 pra prb prc n# len# (P_3 as bs cs)
  = traceFn "repeatPR_3" $
  P_3 (repeatPR pra n# len# as)
      (repeatPR prb n# len# bs)
      (repeatPR prc n# len# cs)

{-# INLINE repeatcPR_3 #-}
repeatcPR_3 pra prb prc n# ns segd (P_3 as bs cs)
  = traceFn "repeatcPR_3" $
  P_3 (repeatcPR pra n# ns segd as)
      (repeatcPR prb n# ns segd bs)
      (repeatcPR prc n# ns segd cs)

{-# INLINE indexPR_3 #-}
indexPR_3 pra prb prc (P_3 as bs cs) i#
  = traceFn "indexPR_3" $
  (indexPR pra as i#, indexPR prb bs i#, indexPR prc cs i#)

{-# INLINE extractPR_3 #-}
extractPR_3 pra prb prc (P_3 as bs cs) i# n# = traceFn "extractPR_3" $
          P_3 (extractPR pra as i# n#)
              (extractPR prb bs i# n#)
              (extractPR prc cs i# n#)

{-# INLINE bpermutePR_3 #-}
bpermutePR_3 pra prb prc (P_3 as bs cs) n# is
  = traceFn "bpermutePR_3" $
  P_3 (bpermutePR pra as n# is)
      (bpermutePR prb bs n# is)
      (bpermutePR prc cs n# is)

{-# INLINE appPR_3 #-}
appPR_3 pra prb prc (P_3 as1 bs1 cs1) (P_3 as2 bs2 cs2)
  = traceFn "appPR_3" $
  P_3 (appPR pra as1 as2)
      (appPR prb bs1 bs2)
      (appPR prc cs1 cs2)

{-# INLINE applPR_3 #-}
applPR_3 pra prb prc is (P_3 as1 bs1 cs1) js (P_3 as2 bs2 cs2)
  = traceFn "applPR_3" $
  P_3 (applPR pra is as1 js as2)
      (applPR prb is bs1 js bs2)
      (applPR prc is cs1 js cs2)

{-# INLINE packPR_3 #-}
packPR_3 pra prb prc (P_3 as bs cs) n# sel#
  = traceFn "packPR_3" $
  P_3 (packPR pra as n# sel#)
      (packPR prb bs n# sel#)
      (packPR prc cs n# sel#)

{-# INLINE combine2PR_3 #-}
combine2PR_3 pra prb prc n# sel (P_3 as1 bs1 cs1)
                                (P_3 as2 bs2 cs2)
  = traceFn "combine2PR_3" $
  P_3 (combine2PR pra n# sel as1 as2)
      (combine2PR prb n# sel bs1 bs2)
      (combine2PR prc n# sel cs1 cs2)

{-# INLINE fromListPR_3 #-}
fromListPR_3 pra prb prc n# xs
  = P_3 (fromListPR pra n# as)
        (fromListPR prb n# bs)
        (fromListPR prc n# cs)
  where
    (as,bs,cs) = unzip3 xs

{-# INLINE nfPR_3 #-}
nfPR_3 pra prb prc (P_3 as bs cs)
  = nfPR pra as
    `seq` nfPR prb bs
    `seq` nfPR prc cs

zip3PA# :: PArray a -> PArray b -> PArray c -> PArray (a,b,c)
{-# INLINE_PA zip3PA# #-}
zip3PA# (PArray n# xs) (PArray _ ys) (PArray _ zs) = PArray n# (P_3 xs ys zs)

dPR_4 :: PR a -> PR b -> PR c -> PR d -> PR (a,b,c,d)
{-# INLINE dPR_4 #-}
dPR_4 pra prb prc prd
  = PR {
      emptyPR      = emptyPR_4 pra prb prc prd
    , replicatePR  = replicatePR_4 pra prb prc prd
    , replicatelPR = replicatelPR_4 pra prb prc prd
    , repeatPR     = repeatPR_4 pra prb prc prd
    , repeatcPR    = repeatcPR_4 pra prb prc prd
    , indexPR      = indexPR_4 pra prb prc prd
    , extractPR    = extractPR_4 pra prb prc prd
    , bpermutePR   = bpermutePR_4 pra prb prc prd
    , appPR        = appPR_4 pra prb prc prd
    , applPR       = applPR_4 pra prb prc prd
    , packPR       = packPR_4 pra prb prc prd
    , combine2PR   = combine2PR_4 pra prb prc prd
    , fromListPR   = fromListPR_4 pra prb prc prd
    , nfPR         = nfPR_4 pra prb prc prd
    }

{-# INLINE emptyPR_4 #-}
emptyPR_4 pra prb prc prd = traceFn "emptyPR_4" $
          P_4 (emptyPR pra)
              (emptyPR prb)
              (emptyPR prc)
              (emptyPR prd)

{-# INLINE replicatePR_4 #-}
replicatePR_4 pra prb prc prd n# (a,b,c,d)
  = traceFn "replicatePR_4" $
  P_4 (replicatePR pra n# a)
      (replicatePR prb n# b)
      (replicatePR prc n# c)
      (replicatePR prd n# d)

{-# INLINE replicatelPR_4 #-}
replicatelPR_4 pra prb prc prd segd (P_4 as bs cs ds)
  = traceFn "replicatelPR_4" $
  P_4 (replicatelPR pra segd as)
      (replicatelPR prb segd bs)
      (replicatelPR prc segd cs)
      (replicatelPR prd segd ds)

{-# INLINE repeatPR_4 #-}
repeatPR_4 pra prb prc prd n# len# (P_4 as bs cs ds)
  = traceFn "repeatPR_4" $
  P_4 (repeatPR pra n# len# as)
      (repeatPR prb n# len# bs)
      (repeatPR prc n# len# cs)
      (repeatPR prd n# len# ds)

{-# INLINE repeatcPR_4 #-}
repeatcPR_4 pra prb prc prd n# ns segd (P_4 as bs cs ds)
  = traceFn "repeatcPR_4" $
  P_4 (repeatcPR pra n# ns segd as)
      (repeatcPR prb n# ns segd bs)
      (repeatcPR prc n# ns segd cs)
      (repeatcPR prd n# ns segd ds)

{-# INLINE indexPR_4 #-}
indexPR_4 pra prb prc prd (P_4 as bs cs ds) i#
  = traceFn "indexPR_4" $
  (indexPR pra as i#,
   indexPR prb bs i#,
   indexPR prc cs i#,
   indexPR prd ds i#)

{-# INLINE extractPR_4 #-}
extractPR_4 pra prb prc prd (P_4 as bs cs ds) i# n#
  = traceFn "extractPR_4" $
    P_4 (extractPR pra as i# n#)
        (extractPR prb bs i# n#)
        (extractPR prc cs i# n#)
        (extractPR prd ds i# n#)

{-# INLINE bpermutePR_4 #-}
bpermutePR_4 pra prb prc prd (P_4 as bs cs ds) n# is
  = traceFn "bpermutePR_4" $
  P_4 (bpermutePR pra as n# is)
      (bpermutePR prb bs n# is)
      (bpermutePR prc cs n# is)
      (bpermutePR prd ds n# is)

{-# INLINE appPR_4 #-}
appPR_4 pra prb prc prd (P_4 as1 bs1 cs1 ds1) (P_4 as2 bs2 cs2 ds2)
  = traceFn "appPR_4" $
  P_4 (appPR pra as1 as2)
      (appPR prb bs1 bs2)
      (appPR prc cs1 cs2)
      (appPR prd ds1 ds2)

{-# INLINE applPR_4 #-}
applPR_4 pra prb prc prd is (P_4 as1 bs1 cs1 ds1) js (P_4 as2 bs2 cs2 ds2)
  = traceFn "applPR_4" $
  P_4 (applPR pra is as1 js as2)
      (applPR prb is bs1 js bs2)
      (applPR prc is cs1 js cs2)
      (applPR prd is ds1 js ds2)

{-# INLINE packPR_4 #-}
packPR_4 pra prb prc prd (P_4 as bs cs ds) n# sel#
  = traceFn "packPR_4" $
  P_4 (packPR pra as n# sel#)
      (packPR prb bs n# sel#)
      (packPR prc cs n# sel#)
      (packPR prd ds n# sel#)

{-# INLINE combine2PR_4 #-}
combine2PR_4 pra prb prc prd n# sel (P_4 as1 bs1 cs1 ds1)
                                    (P_4 as2 bs2 cs2 ds2)
  = traceFn "combine2PR_4" $
  P_4 (combine2PR pra n# sel as1 as2)
      (combine2PR prb n# sel bs1 bs2)
      (combine2PR prc n# sel cs1 cs2)
      (combine2PR prd n# sel ds1 ds2)

{-# INLINE fromListPR_4 #-}
fromListPR_4 pra prb prc prd n# xs
  = P_4 (fromListPR pra n# as)
        (fromListPR prb n# bs)
        (fromListPR prc n# cs)
        (fromListPR prd n# ds)
  where
    (as,bs,cs,ds) = L.unzip4 xs

{-# INLINE nfPR_4 #-}
nfPR_4 pra prb prc prd (P_4 as bs cs ds)
  = nfPR pra as
    `seq` nfPR prb bs
    `seq` nfPR prc cs
    `seq` nfPR prd ds

dPR_5 :: PR a -> PR b -> PR c -> PR d -> PR e -> PR (a,b,c,d,e)
{-# INLINE dPR_5 #-}
dPR_5 pra prb prc prd pre
  = PR {
      emptyPR      = emptyPR_5 pra prb prc prd pre
    , replicatePR  = replicatePR_5 pra prb prc prd pre
    , replicatelPR = replicatelPR_5 pra prb prc prd pre
    , repeatPR     = repeatPR_5 pra prb prc prd pre
    , repeatcPR    = repeatcPR_5 pra prb prc prd pre
    , indexPR      = indexPR_5 pra prb prc prd pre
    , extractPR    = extractPR_5 pra prb prc prd pre
    , bpermutePR   = bpermutePR_5 pra prb prc prd pre
    , appPR        = appPR_5 pra prb prc prd pre
    , applPR       = applPR_5 pra prb prc prd pre
    , packPR       = packPR_5 pra prb prc prd pre
    , combine2PR   = combine2PR_5 pra prb prc prd pre
    , fromListPR   = fromListPR_5 pra prb prc prd pre
    , nfPR         = nfPR_5 pra prb prc prd pre
    }

{-# INLINE emptyPR_5 #-}
emptyPR_5 pra prb prc prd pre
  = traceFn "emptyPR_5" $
  P_5 (emptyPR pra)
      (emptyPR prb)
      (emptyPR prc)
      (emptyPR prd)
      (emptyPR pre)

{-# INLINE replicatePR_5 #-}
replicatePR_5 pra prb prc prd pre n# (a,b,c,d,e)
  = traceFn "replicatePR_5" $
  P_5 (replicatePR pra n# a)
      (replicatePR prb n# b)
      (replicatePR prc n# c)
      (replicatePR prd n# d)
      (replicatePR pre n# e)

{-# INLINE replicatelPR_5 #-}
replicatelPR_5 pra prb prc prd pre segd (P_5 as bs cs ds es)
  = traceFn "replicatelPR_5" $
  P_5 (replicatelPR pra segd as)
      (replicatelPR prb segd bs)
      (replicatelPR prc segd cs)
      (replicatelPR prd segd ds)
      (replicatelPR pre segd es)

{-# INLINE repeatPR_5 #-}
repeatPR_5 pra prb prc prd pre n# len# (P_5 as bs cs ds es)
  = traceFn "repeatPR_5" $
  P_5 (repeatPR pra n# len# as)
      (repeatPR prb n# len# bs)
      (repeatPR prc n# len# cs)
      (repeatPR prd n# len# ds)
      (repeatPR pre n# len# es)

{-# INLINE repeatcPR_5 #-}
repeatcPR_5 pra prb prc prd pre n# ns segd (P_5 as bs cs ds es)
  = traceFn "repeatcPR_5" $
  P_5 (repeatcPR pra n# ns segd as)
      (repeatcPR prb n# ns segd bs)
      (repeatcPR prc n# ns segd cs)
      (repeatcPR prd n# ns segd ds)
      (repeatcPR pre n# ns segd es)

{-# INLINE indexPR_5 #-}
indexPR_5 pra prb prc prd pre (P_5 as bs cs ds es) i#
  = traceFn "indexPR_5" $
  (indexPR pra as i#,
   indexPR prb bs i#,
   indexPR prc cs i#,
   indexPR prd ds i#,
   indexPR pre es i#)

{-# INLINE extractPR_5 #-}
extractPR_5 pra prb prc prd pre (P_5 as bs cs ds es) i# n#
  = traceFn "extractPR_5" $
    P_5 (extractPR pra as i# n#)
        (extractPR prb bs i# n#)
        (extractPR prc cs i# n#)
        (extractPR prd ds i# n#)
        (extractPR pre es i# n#)

{-# INLINE bpermutePR_5 #-}
bpermutePR_5 pra prb prc prd pre (P_5 as bs cs ds es) n# is
  = traceFn "bpermutePR_5" $
  P_5 (bpermutePR pra as n# is)
      (bpermutePR prb bs n# is)
      (bpermutePR prc cs n# is)
      (bpermutePR prd ds n# is)
      (bpermutePR pre es n# is)

{-# INLINE appPR_5 #-}
appPR_5 pra prb prc prd pre (P_5 as1 bs1 cs1 ds1 es1)
                            (P_5 as2 bs2 cs2 ds2 es2)
  = traceFn "appPR_5" $
  P_5 (appPR pra as1 as2)
      (appPR prb bs1 bs2)
      (appPR prc cs1 cs2)
      (appPR prd ds1 ds2)
      (appPR pre es1 es2)

{-# INLINE applPR_5 #-}
applPR_5 pra prb prc prd pre is (P_5 as1 bs1 cs1 ds1 es1)
                             js (P_5 as2 bs2 cs2 ds2 es2)
  = traceFn "applPR_5" $
  P_5 (applPR pra is as1 js as2)
      (applPR prb is bs1 js bs2)
      (applPR prc is cs1 js cs2)
      (applPR prd is ds1 js ds2)
      (applPR pre is es1 js es2)

{-# INLINE packPR_5 #-}
packPR_5 pra prb prc prd pre (P_5 as bs cs ds es) n# sel#
  = traceFn "packPR_5" $
  P_5 (packPR pra as n# sel#)
      (packPR prb bs n# sel#)
      (packPR prc cs n# sel#)
      (packPR prd ds n# sel#)
      (packPR pre es n# sel#)

{-# INLINE combine2PR_5 #-}
combine2PR_5 pra prb prc prd pre n# sel (P_5 as1 bs1 cs1 ds1 es1)
                                        (P_5 as2 bs2 cs2 ds2 es2)
  = traceFn "combine2PR_5" $
  P_5 (combine2PR pra n# sel as1 as2)
      (combine2PR prb n# sel bs1 bs2)
      (combine2PR prc n# sel cs1 cs2)
      (combine2PR prd n# sel ds1 ds2)
      (combine2PR pre n# sel es1 es2)

{-# INLINE fromListPR_5 #-}
fromListPR_5 pra prb prc prd pre n# xs
  = P_5 (fromListPR pra n# as)
        (fromListPR prb n# bs)
        (fromListPR prc n# cs)
        (fromListPR prd n# ds)
        (fromListPR pre n# es)
  where
    (as,bs,cs,ds,es) = L.unzip5 xs

{-# INLINE nfPR_5 #-}
nfPR_5 pra prb prc prd pre (P_5 as bs cs ds es)
  = nfPR pra as
    `seq` nfPR prb bs
    `seq` nfPR prc cs
    `seq` nfPR prd ds
    `seq` nfPR pre es

data Sum2 a b = Alt2_1 a | Alt2_2 b
data Sum3 a b c = Alt3_1 a | Alt3_2 b | Alt3_3 c

data instance PData (Sum2 a b)
  = PSum2 Sel2 (PData a) (PData b)

dPR_Sum2 :: PR a -> PR b -> PR (Sum2 a b)
{-# INLINE dPR_Sum2 #-}
dPR_Sum2 pra prb = PR {
                     emptyPR      = emptyPR_Sum2 pra prb
                   , replicatePR  = replicatePR_Sum2 pra prb
                   , replicatelPR = replicatelPR_Sum2 pra prb
                   , repeatPR     = repeatPR_Sum2 pra prb
                   , indexPR      = indexPR_Sum2 pra prb
                   , appPR        = appPR_Sum2 pra prb 
                   , packPR       = packPR_Sum2 pra prb 
                   , combine2PR   = combine2PR_Sum2 pra prb 
                   }
 
{-# INLINE emptyPR_Sum2 #-}
emptyPR_Sum2 pra prb
  = traceFn "emptyPR_Sum2" $
  PSum2 (mkSel2 U.empty U.empty 0 0) (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Sum2 #-}
replicatePR_Sum2 pra prb n# (Alt2_1 x)
  = traceFn "replicatePR_Sum2" $
    PSum2 (mkSel2 (U.replicate (I# n#) 0)
                  (U.enumFromStepLen 0 1 (I# n#))
                  (I# n#) 0)
          (replicatePR pra n# x)
          (emptyPR prb)
replicatePR_Sum2 pra prb n# (Alt2_2 x)
  = traceFn "replicatePR_Sum2" $
    PSum2 (mkSel2 (U.replicate (I# n#) 1)
                  (U.enumFromStepLen 0 1 (I# n#))
                  0 (I# n#))
          (emptyPR pra)
          (replicatePR prb n# x)

{-# INLINE replicatelPR_Sum2 #-}
replicatelPR_Sum2 pra prb segd (PSum2 sel as bs)
  = traceFn "replicatelPR_Sum2" $
    PSum2 sel' as' bs'
  where
    tags      = tagsSel2 sel
    tags'     = U.replicate_s segd tags
    sel'      = tagsToSel2 tags'

    lens      = U.lengthsSegd segd

    asegd     = U.lengthsToSegd
              . U.pack lens
              $ U.pick tags 0
    bsegd     = U.lengthsToSegd
              . U.pack lens
              $ U.pick tags 1

    as'       = replicatelPR pra asegd as
    bs'       = replicatelPR prb bsegd bs

{-# INLINE repeatPR_Sum2 #-}
repeatPR_Sum2 pra prb m# n# (PSum2 sel as bs)
  = traceFn "repeatPR_Sum2" $
    PSum2 sel' as' bs'
  where
    sel' = tagsToSel2
         . U.repeat (I# m#) (I# n#)
         $ tagsSel2 sel

    as'  = repeatPR pra m# (elementsSel2_0# sel) as
    bs'  = repeatPR prb n# (elementsSel2_1# sel) bs

{-# INLINE indexPR_Sum2 #-}
indexPR_Sum2 pra prb (PSum2 sel as bs) i#
  = traceFn "indexPR_Sum2" $
  case indicesSel2 sel U.!: I# i# of
    I# k# -> case tagsSel2 sel U.!: I# i# of
               0 -> Alt2_1 (indexPR pra as k#)
               _ -> Alt2_2 (indexPR prb bs k#)

{-# INLINE appPR_Sum2 #-}
appPR_Sum2 pra prb (PSum2 sel1 as1 bs1)
                   (PSum2 sel2 as2 bs2)
  = traceFn "appPR_Sum2" $           
    PSum2 sel (appPR pra as1 as2)
              (appPR prb bs1 bs2)
  where
    sel = tagsToSel2
        $ tagsSel2 sel1 U.+:+ tagsSel2 sel2

{-# INLINE packPR_Sum2 #-}
packPR_Sum2 pra prb (PSum2 sel as bs) m# flags
  = traceFn "packPR_Sum2" $
    PSum2 sel' as' bs'
  where
    tags   = tagsSel2 sel
    tags'  = U.pack tags flags
    sel'   = tagsToSel2 tags'

    aFlags = U.pack flags (U.pick tags 0)
    bFlags = U.pack flags (U.pick tags 1)

    as'    = packPR pra as (elementsSel2_0# sel') aFlags
    bs'    = packPR prb bs (elementsSel2_1# sel') bFlags

    _dummy :: Int#
    _dummy = m#

combine2PR_Sum2 :: PR a -> PR b -> T_combine2PR (Sum2 a b)
{-# INLINE combine2PR_Sum2 #-}
combine2PR_Sum2 pra prb n# sel (PSum2 sel1 as1 bs1)
                               (PSum2 sel2 as2 bs2)
  = traceFn "combine2PR_Sum2" $
    PSum2 sel' as bs
  where
    tags  = tagsSel2 sel
    tags' = U.combine (U.pick tags 0) (tagsSel2 sel1) (tagsSel2 sel2)
    sel'  = tagsToSel2 tags'

    asel = tagsToSel2 (U.pack tags (U.pick tags' 0))
    bsel = tagsToSel2 (U.pack tags (U.pick tags' 1))

    as   = combine2PR pra (elementsSel2_0# sel') asel as1 as2
    bs   = combine2PR prb (elementsSel2_1# sel') bsel bs1 bs2

{-
data instance PData (Sum2 a b)
  = PSum2 (U.Array Int) (U.Array Int) Int# (PData a)
                                      Int# (PData b)

data instance PData (Sum3 a b c)
  = PSum3 (U.Array Int) (U.Array Int) Int# (PData a)
                                      Int# (PData b)
                                      Int# (PData c)

dPR_Sum2 :: PR a -> PR b -> PR (Sum2 a b)
{-# INLINE dPR_Sum2 #-}
dPR_Sum2 pra prb = PR {
                     emptyPR      = emptyPR_Sum2 pra prb
                   , replicatePR  = replicatePR_Sum2 pra prb
                   , replicatelPR = replicatelPR_Sum2 pra prb
                   , repeatPR     = repeatPR_Sum2 pra prb
                   , indexPR      = indexPR_Sum2 pra prb
                   , appPR        = appPR_Sum2 pra prb 
                   , packPR       = packPR_Sum2 pra prb 
                   , combine2PR   = combine2PR_Sum2 pra prb 
                   }
 
{-# INLINE emptyPR_Sum2 #-}
emptyPR_Sum2 pra prb
  = traceFn "emptyPR_Sum2" $
  PSum2 U.empty U.empty 0# (emptyPR pra)
                        0# (emptyPR prb)

{-# INLINE replicatePR_Sum2 #-}
replicatePR_Sum2 pra prb n# (Alt2_1 x)
  = traceFn "replicatePR_Sum2" $
    PSum2 (U.replicate (I# n#) 0)
          (U.enumFromStepLen 0 1 (I# n#))
          n# (replicatePR pra n# x)
          0# (emptyPR prb)
replicatePR_Sum2 pra prb n# (Alt2_2 x)
  = traceFn "replicatePR_Sum2" $
    PSum2 (U.replicate (I# n#) 1)
          (U.enumFromStepLen 0 1 (I# n#))
          0# (emptyPR pra)
          n# (replicatePR prb n# x)

{-# INLINE replicatelPR_Sum2 #-}
replicatelPR_Sum2 pra prb segd (PSum2 sel is _ as _ bs)
  = traceFn "replicatelPR_Sum2" $
    PSum2 sel' is' (elementsSegd# asegd) as'
                   (elementsSegd# bsegd) bs'
  where
    sel'      = U.replicate_s segd sel
    is'       = U.selectorToIndices2 sel'

    lens      = U.lengthsSegd segd

    asegd     = U.lengthsToSegd
              . U.pack lens
              $ U.pick sel 0
    bsegd     = U.lengthsToSegd
              . U.pack lens
              $ U.pick sel 1

    as'       = replicatelPR pra asegd as
    bs'       = replicatelPR prb bsegd bs

{-# INLINE repeatPR_Sum2 #-}
repeatPR_Sum2 pra prb m# n# (PSum2 sel is an# as bn# bs)
  = traceFn "repeatPR_Sum2" $
    PSum2 sel' is' (m# *# an#) as'
                   (m# *# bn#) bs'
  where
    sel' = U.repeat (I# m#) (I# n#) sel
    is'  = U.selectorToIndices2 sel'

    !an'# = m# *# an#
    !bn'# = m# *# bn#

    as'  = repeatPR pra m# an# as
    bs'  = repeatPR prb n# bn# bs

{-# INLINE indexPR_Sum2 #-}
indexPR_Sum2 pra prb (PSum2 sel is _ as _ bs) i#
  = traceFn "indexPR_Sum2" $
  case is U.!: I# i# of
    I# k# -> case sel U.!: I# i# of
               0 -> Alt2_1 (indexPR pra as k#)
               _ -> Alt2_2 (indexPR prb bs k#)

{-# INLINE appPR_Sum2 #-}
appPR_Sum2 pra prb (PSum2 sel1 _ an1# as1 bn1# bs1)
                   (PSum2 sel2 _ an2# as2 bn2# bs2)
  = traceFn "appPR_Sum2" $           
    PSum2 sel is (an1# +# an2#) (appPR pra as1 as2)
                 (bn1# +# bn2#) (appPR prb bs1 bs2)
  where
    sel = sel1 U.+:+ sel2
    is  = U.selectorToIndices2 sel

{-# INLINE packPR_Sum2 #-}
packPR_Sum2 pra prb (PSum2 sel _ an# as bn# bs) m# flags
  = traceFn "packPR_Sum2" $
    PSum2 sel' is' an'# as' bn'# bs'
  where
    sel'   = U.pack sel flags
    is'    = U.selectorToIndices2 sel'

    aFlags = U.pack flags (U.pick sel 0)
    bFlags = U.pack flags (U.pick sel 1)

    !bn'#   = case U.count bFlags True of I# n# -> n#
    !an'#   = m# -# bn'#

    as'    = packPR pra as an'# aFlags
    bs'    = packPR prb bs bn'# bFlags

combine2PR_Sum2 :: PR a -> PR b -> T_combine2PR (Sum2 a b)
{-# INLINE combine2PR_Sum2 #-}
combine2PR_Sum2 pra prb n# sel is (PSum2 sel1 _ an1# as1 bn1# bs1)
                                  (PSum2 sel2 _ an2# as2 bn2# bs2)
  = traceFn "combine2PR_Sum2" $
    PSum2 sel' is' an# as bn# bs
  where
    sel' = U.combine (U.pick sel 0) sel1 sel2
    is'  = U.selectorToIndices2 sel'
   
    !an# = an1# +# an2#
    !bn# = bn1# +# bn2#

    asel = U.pack sel (U.pick sel' 0)
    bsel = U.pack sel (U.pick sel' 1)

    as   = combine2PR pra an# asel (U.selectorToIndices2 asel) as1 as2
    bs   = combine2PR prb bn# bsel (U.selectorToIndices2 bsel) bs1 bs2

{-
= traceFn "combine2PR_Sum2" $                
     case   (sel'Bool, nsel'Bool) of
       (s1#, s2#) ->  traceArgs ("combinePR_Sum\nn  = " ++ show (I# n#)  ++ "\n" ++
                                 "m1 = " ++ show (I# m1#)  ++ "\n" ++
                                 "m2 = " ++ show (I# m2#)  ++ "\n" ++
                                 "as# = " ++ show (I# (lengthPR pra as1)) ++ " " ++ show (I# (lengthPR pra as2)) ++ "\n" ++
                                 "bs# = " ++ show (I# (lengthPR prb bs1)) ++ " " ++ show (I# (lengthPR prb bs2)) ++ "\n" ++
                                 "sel = " ++ show sel#  ++ "\n" ++
                                 "sel1 = " ++ show sel1#  ++ "\n" ++
                                 "sel2 = " ++ show sel2#  ++ "\n" ++
                                 "s1# = " ++ show s1#  ++ "\n" ++
                                 "s2# = " ++ show s2#  ++ "\n" ++
                                 "selB = " ++ show sel'Bool  ++ "\n" ++
                                 "nselB = " ++ show nsel'Bool  ++ "\n" ++
                                 "sel' = " ++ show sel'  ++ "\n"
                             )
                          $ 
                         PSum2  n# sel' (error "combine2PR_Sum2 index nyi") as' bs'
                      where     
                        !as# = lengthPR pra as1 +#   lengthPR pra as2
                        !bs# = lengthPR prb bs1 +#   lengthPR prb bs2
                        asel = packPA_Int# sel# as# s1#
                        bsel = packPA_Int# sel# bs# s2#
                        as' = trace ("cb1: " ++ show asel) $ combine2PR pra as# asel  is# as1 as2 
                        bs' = trace ("cb2: " ++ show bsel) $ combine2PR prb bs# bsel is# bs1 bs2
     where
       sel' = combine2PA_Int# n# sel# is#  sel1# sel2#
       sel'Bool  = selectPA_Int# sel' 0#
       nsel'Bool = selectPA_Int# sel' 1#
-}

dPR_Sum3 :: PR a -> PR b -> PR c -> PR (Sum3 a b c)
{-# INLINE dPR_Sum3 #-}
dPR_Sum3 pra prb prc
  = PR {
     emptyPR     = emptyPR_Sum3 pra prb prc
   , replicatePR = replicatePR_Sum3 pra prb prc
   , indexPR     = indexPR_Sum3 pra prb prc
   }

{-# INLINE emptyPR_Sum3 #-}
emptyPR_Sum3 pra prb prc        
  = traceFn "emptyPR_Sum3\n" $
  PSum3 U.empty U.empty 0# (emptyPR pra)
                        0# (emptyPR prb)
                        0# (emptyPR prc)

{-# INLINE replicatePR_Sum3 #-}
replicatePR_Sum3 pra prb prc n# (Alt3_1 x)
  = traceFn "replicatePR_Sum3\n" $
  PSum3 (U.replicate (I# n#) 0)
        (U.enumFromStepLen 0 1 (I# n#))
        n# (replicatePR pra n# x)
        0# (emptyPR prb)
        0# (emptyPR prc)
replicatePR_Sum3 pra prb prc n# (Alt3_2 x)
  = traceFn "replicatePR_Sum3\n" $
  PSum3 (U.replicate (I# n#) 1)
        (U.enumFromStepLen 0 1 (I# n#))
        0# (emptyPR pra)
        n# (replicatePR prb n# x)
        0# (emptyPR prc)
replicatePR_Sum3 pra prb prc n# (Alt3_3 x)
  = traceFn "replicatePR_Sum3\n" $
  PSum3 (U.replicate (I# n#) 2)
        (U.enumFromStepLen 0 1 (I# n#))
        0# (emptyPR pra)
        0# (emptyPR prb)
        n# (replicatePR prc n# x)

{-# INLINE indexPR_Sum3 #-}
indexPR_Sum3 pra prb prc (PSum3 sel is _ as _ bs _ cs) i#
  = traceFn "indexPR_Sum3\n" $
  case is U.!: I# i# of
    I# k# -> case sel U.!: I# i# of
               0 -> Alt3_1 (indexPR pra as k#)
               1 -> Alt3_2 (indexPR prb bs k#)
               _ -> Alt3_3 (indexPR prc cs k#)
-}

data instance PData (PArray a) = PNested U.Segd (PData a)

dPR_PArray :: PR a -> PR (PArray a)
{-# INLINE dPR_PArray #-}
dPR_PArray pr = PR {
                  emptyPR      = emptyPR_PArray pr
                , replicatePR  = replicatePR_PArray pr
                , replicatelPR = replicatelPR_PArray pr
                , repeatPR     = repeatPR_PArray pr
                , indexPR      = indexPR_PArray pr
                , extractPR    = extractPR_PArray pr
                , bpermutePR   = bpermutePR_PArray pr
                , appPR        = appPR_PArray pr
                , applPR       = applPR_PArray pr
                , packPR       = packPR_PArray pr
                , combine2PR   = combine2PR_PArray pr
                }

{-
{-# INLINE nested_lengthPA #-}
nested_lengthPA xss = traceFn "nested_lengthPA\n" $
                I# (lengthPR_PArray xss)
-}


{-# INLINE emptyPR_PArray #-}
emptyPR_PArray pr = traceFn "emptyPR_PArray\n" $
        PNested (U.mkSegd U.empty U.empty 0) (emptyPR pr)

{-# INLINE replicatePR_PArray #-}
replicatePR_PArray pr n# (PArray m# xs)
  = traceFn "replicatePR_PArray\n" $
  PNested (U.mkSegd (U.replicate (I# n#) (I# m#))
                    (U.enumFromStepLen 0 (I# m#) (I# n#))
                    (I# n# * I# m#))
          (repeatPR pr n# m# xs)

{-# INLINE indexPR_PArray #-}
indexPR_PArray pr (PNested segd xs) i#
  = case U.lengthsSegd segd U.!: I# i# of { I# n# ->
    case U.indicesSegd segd U.!: I# i# of { I# k# ->
    PArray n# (extractPR pr xs k# n#) }}

{-# INLINE extractPR_PArray #-}
extractPR_PArray pr (PNested segd xs) i# n#
  = case U.indicesSegd segd U.!: I# i# of
      I# k# -> PNested segd' (extractPR pr xs k# (elementsSegd# segd'))
  where
    segd' = U.lengthsToSegd
          $ U.extract (U.lengthsSegd segd) (I# i#) (I# n#)

bpermutePR_PArray :: PR a -> T_bpermutePR (PArray a)
{-# INLINE bpermutePR_PArray #-}
bpermutePR_PArray pr (PNested segd xs) n# is
  = traceFn "bpermutePR_PArray\n" $
    PNested segd' (bpermutePR pr xs (elementsSegd# segd') flat_is)
  where
    segd'   = U.lengthsToSegd
            $ U.bpermute (U.lengthsSegd segd) is

    starts  = U.indicesSegd segd'
    lens    = U.lengthsSegd segd'
    ends    = U.zipWith (\x y -> x + y - 1) starts lens
    flat_is = U.enumFromToEach (U.elementsSegd segd') (U.zip starts ends)

{-# INLINE appPR_PArray #-}
appPR_PArray pr (PNested xsegd xs) (PNested ysegd ys)
  = traceFn "appPR_PArray\n" $             
    PNested (U.lengthsToSegd (U.lengthsSegd xsegd U.+:+ U.lengthsSegd ysegd))
            (appPR pr xs ys)

{-# INLINE applPR_PArray #-}
applPR_PArray pr segd1 (PNested xsegd xs) segd2 (PNested ysegd ys)
  = PNested segd (applPR pr xsegd' xs ysegd' ys)
  where
    segd = U.lengthsToSegd
         $ U.append_s segd1 (U.lengthsSegd xsegd) segd2 (U.lengthsSegd ysegd)

    xsegd' = U.lengthsToSegd
           $ U.sum_s segd1 (U.lengthsSegd xsegd)
    ysegd' = U.lengthsToSegd
           $ U.sum_s segd2 (U.lengthsSegd ysegd)

{-# INLINE repeatPR_PArray #-}
repeatPR_PArray pr n# len# (PNested segd xs)
  = traceFn "repeatPR_PArray\n" $
  PNested segd' (repeatPR pr n# (elementsSegd# segd) xs)
  where
    segd' = U.lengthsToSegd (U.repeat (I# n#) (I# len#) (U.lengthsSegd segd))

{-# INLINE replicatelPR_PArray #-}
replicatelPR_PArray pr segd (PNested xsegd xs)
  = traceFn "replicatelPR_PArray\n" $
  PNested xsegd' xs'
  where
    xsegd' = U.lengthsToSegd
           $ U.replicate_s segd (U.lengthsSegd xsegd)

    xs'    = repeatcPR pr (elementsSegd# xsegd')
                          (U.lengthsSegd segd)
                          xsegd xs

packPR_PArray :: PR a -> T_packPR (PArray a)
{-# INLINE packPR_PArray #-}
packPR_PArray pr (PNested segd xs) n# flags
  = traceFn "packPR_PArray\n" $
  PNested segd' xs'
  where
    segd' = U.lengthsToSegd
          $ U.pack (U.lengthsSegd segd) flags

    xs'   = packPR pr xs (elementsSegd# segd') (U.replicate_s segd flags)

combine2PR_PArray :: PR a -> T_combine2PR (PArray a)
{-# INLINE combine2PR_PArray #-}
combine2PR_PArray pr n# sel (PNested xsegd xs) (PNested ysegd ys)
  = traceFn "combine2PR_PArray\n" $
  PNested segd xys
  where
    tags = tagsSel2 sel

    segd = U.lengthsToSegd
         $ U.combine (U.pick tags 0) (U.lengthsSegd xsegd) (U.lengthsSegd ysegd)

    sel' = tagsToSel2
         $ U.replicate_s segd tags

    xys  = combine2PR pr (elementsSegd# segd) sel' xs ys

segdPA# :: PArray (PArray a) -> U.Segd
{-# INLINE_PA segdPA# #-}
segdPA# (PArray _ (PNested segd _)) = segd

concatPA# :: PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA# #-}
concatPA# (PArray _ (PNested segd xs)) = PArray (elementsSegd# segd) xs

segmentPA# :: Int# -> U.Segd -> PArray a -> PArray (PArray a)
{-# INLINE_PA segmentPA# #-}
segmentPA# n# segd (PArray _ xs) = PArray n# (PNested segd xs)

copySegdPA# :: PArray (PArray a) -> PArray b -> PArray (PArray b)
{-# INLINE copySegdPA# #-}
copySegdPA# (PArray n# (PNested segd _)) (PArray _ xs)
  = PArray n# (PNested segd xs)

