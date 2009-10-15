{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Repr (
  PData(..),
  Void, void, pvoid, fromVoid,
{-
  Wrap(..),
  Sum2(..), Sum3(..), 
-}

  punit,
  Wrap(..),

  zipPA#, unzipPA#, zip3PA#,
  Sum2(..), {-Sum3(..),-} {-dPR_Sum3,-}

  segdPA#, concatPA#, segmentPA#, copySegdPA#
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

fromVoid :: a
fromVoid = error "fromPRepr Data.Array.Parallel.void"

data instance PData Void

pvoid :: PData Void
pvoid = error "Data.Array.Parallel.PData Void"

instance PR Void where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR<Void>" $ pvoid

  {-# INLINE replicatePR #-}
  replicatePR _ _ = traceFn "replicatePR<Void>" $ pvoid

  {-# INLINE replicatelPR #-}
  replicatelPR segd _ = traceFn "replicatelPR<Void>" $ pvoid

  {-# INLINE repeatPR #-}
  repeatPR _ _ _ = traceFn "repeatPR<Void>" $ pvoid

  {-# INLINE repeatcPR #-}
  repeatcPR _ _ _ _ = traceFn "repeatcPR<Void>" $ pvoid

  {-# INLINE indexPR #-}
  indexPR _ _ = traceFn "indexPR<Void>" $ void

  {-# INLINE extractPR #-}
  extractPR  _ _ _ = traceFn "extractPR<Void>" $ pvoid

  {-# INLINE bpermutePR #-}
  bpermutePR _ _ _ = traceFn "bpermutePR<Void>" $ pvoid

  {-# INLINE appPR #-}
  appPR  _ _ = traceFn "appPR<Void>" $ pvoid

  {-# INLINE applPR #-}
  applPR _ _ _ _ = traceFn "applPR<Void>" $ pvoid

  {-# INLINE packPR #-}
  packPR _ _ _ = traceFn "packPR<Void>" $ pvoid

  {-# INLINE combine2PR #-}
  combine2PR _ _ _ _ = traceFn "combine2PR<Void>" $ pvoid

  {-# INLINE fromListPR #-}
  fromListPR _ _ = pvoid

  {-# INLINE nfPR #-}
  nfPR _ = ()

type instance PRepr Void = Void

instance PA Void where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


data instance PData () = PUnit

punit :: PData ()
punit = PUnit

instance PR () where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR<Unit>" $ PUnit

  {-# INLINE replicatePR #-}
  replicatePR n# () = 
    traceFn "replicatePR<Unit>" $
    traceArgs ("replicatePR len = " ++ show (I# n#)) $
    PUnit

  {-# INLINE replicatelPR #-}
  replicatelPR segd u =
    traceFn "replicatelPR<Unit>" 
    traceArgs ("replicatelPR args len = " ++ (show (U.elementsSegd segd))) $
    u

  {-# INLINE repeatPR #-}
  repeatPR _ _ u = traceFn "repeatPR<Unit>" $ u

  {-# INLINE repeatcPR #-}
  repeatcPR _ _ _ u = traceFn "repeatcPR<Unit>" $ u

  {-# INLINE indexPR #-}
  indexPR PUnit _ = traceFn "indexPR<Unit>" $ ()

  {-# INLINE extractPR #-}
  extractPR u _ _ = traceFn "extractPR<Unit>" $ u

  {-# INLINE bpermutePR #-}
  bpermutePR u _ _ = traceFn "bpermutePR<Unit>" $ u

  {-# INLINE appPR #-}
  appPR u v = traceFn "appPR<Unit>" (u `seq` v)

  {-# INLINE applPR #-}
  applPR _ u _ v = traceFn "applPR<Unit>" (u `seq` v)

  {-# INLINE packPR #-}
  packPR u _ _ = traceFn "packPR<Unit>" $ u

  {-# INLINE combine2PR #-}
  combine2PR _ _ u v = traceFn "combine2PR<Unit>" (u `seq` v)

  {-# INLINE fromListPR #-}
  fromListPR _ xs = foldr seq PUnit xs

  {-# INLINE nfPR #-}
  nfPR PUnit = ()


newtype Wrap a = Wrap { unWrap :: a }

newtype instance PData (Wrap a) = PWrap (PData a)

instance PA a => PR (Wrap a) where
  {-# INLINE emptyPR #-}
  emptyPR = PWrap emptyPD

  {-# INLINE replicatePR #-}
  replicatePR n# (Wrap x) = PWrap (replicatePD n# x)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PWrap xs) = PWrap (replicatelPD segd xs)

  {-# INLINE repeatPR #-}
  repeatPR m# n# (PWrap xs) = PWrap (repeatPD m# n# xs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (PWrap xs) = PWrap (repeatcPD n# ns segd xs)

  {-# INLINE indexPR #-}
  indexPR (PWrap xs) i# = Wrap (indexPD xs i#)

  {-# INLINE extractPR #-}
  extractPR (PWrap xs) i# n# = PWrap (extractPD xs i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (PWrap xs) n# is = PWrap (bpermutePD xs n# is)

  {-# INLINE appPR #-}
  appPR (PWrap xs) (PWrap ys) = PWrap (appPD xs ys)

  {-# INLINE applPR #-}
  applPR xsegd (PWrap xs) ysegd (PWrap ys)
    = PWrap (applPD xsegd xs ysegd ys)

  {-# INLINE packPR #-}
  packPR (PWrap xs) n# bs = PWrap (packPD xs n# bs)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PWrap xs) (PWrap ys)
    = PWrap (combine2PD n# sel xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PWrap (fromListPD n# (map unWrap xs))

  {-# INLINE nfPR #-}
  nfPR (PWrap xs) = nfPD xs


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

instance (PR a, PR b) => PR (a,b) where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR<(a,b)>" $
            P_2 emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b) = 
    traceFn "replicatePR<(a,b)>" $
    traceArgs ("replicatePR args len = "  ++ (show (I# n#))) $
    P_2 (replicatePR n# a)
        (replicatePR n# b)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_2 as bs)
    = traceFn "replicatelPR<(a,b)>" $
      P_2 (replicatelPR segd as)
          (replicatelPR segd bs) 

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_2 as bs)
    = traceFn "repeatPR<(a,b)>" $
      P_2 (repeatPR n# len# as)
          (repeatPR n# len# bs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_2 as bs)
    = traceFn "repeatcPR<(a,b)>" $
      P_2 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)

  {-# INLINE indexPR #-}
  indexPR (P_2 as bs) i# = traceFn "indexPR<(a,b)>" $
                                     (indexPR as i#, indexPR bs i#)

  {-# INLINE extractPR #-}
  extractPR (P_2 as bs) i# n# = traceFn "extractPR<(a,b)>"
                                          P_2 (extractPR as i# n#)
                                              (extractPR bs i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_2 as bs) n# is
    = traceFn "bpermutePR<(a,b)>" $
      P_2 (bpermutePR as n# is) (bpermutePR bs n# is)

  {-# INLINE appPR #-}
  appPR (P_2 as1 bs1) (P_2 as2 bs2)
    = P_2 (appPR as1 as2) (appPR bs1 bs2)

  {-# INLINE applPR #-}
  applPR is (P_2 as1 bs1) js (P_2 as2 bs2)
    = traceFn "applPR<(a,b)>" $
      P_2 (applPR is as1 js as2)
          (applPR is bs1 js bs2)

  {-# INLINE packPR #-}
  packPR (P_2 as bs) n# sel# = traceFn "packPR<(a,b)>" $
         P_2 (packPR as n# sel#)
             (packPR bs n# sel#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_2 as1 bs1) (P_2 as2 bs2)
    = traceFn "combine2PR<(a,b)>" $
      P_2 (combine2PR n# sel as1 as2)
          (combine2PR n# sel bs1 bs2)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = P_2 (fromListPR n# as)
                         (fromListPR n# bs)
    where
      (as,bs) = unzip xs

  {-# INLINE nfPR #-}
  nfPR (P_2 as bs) = nfPR as `seq` nfPR bs

zipPA# :: PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA# #-}
zipPA# (PArray n# xs) (PArray _ ys) = PArray n# (P_2 xs ys)

unzipPA# :: PArray (a,b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA# #-}
unzipPA# (PArray n# (P_2 xs ys)) = (PArray n# xs, PArray n# ys)


instance (PR a, PR b, PR c) => PR (a,b,c) where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR<(a,b,c)>" $
          P_3 emptyPR emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b,c)
    = traceFn "replicatePR<(a,b,c)>" $
      P_3 (replicatePR n# a)
          (replicatePR n# b)
          (replicatePR n# c)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_3 as bs cs)
    = traceFn "replicatelPR<(a,b,c)>" $
      P_3 (replicatelPR segd as)
          (replicatelPR segd bs)
          (replicatelPR segd cs)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_3 as bs cs)
    = traceFn "repeatPR<(a,b,c)>" $
      P_3 (repeatPR n# len# as)
          (repeatPR n# len# bs)
          (repeatPR n# len# cs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_3 as bs cs)
    = traceFn "repeatcPR<(a,b,c)>" $
      P_3 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)
          (repeatcPR n# ns segd cs)

  {-# INLINE indexPR #-}
  indexPR (P_3 as bs cs) i#
    = traceFn "indexPR<(a,b,c)>" $
      (indexPR as i#, indexPR bs i#, indexPR cs i#)

  {-# INLINE extractPR #-}
  extractPR (P_3 as bs cs) i# n# = traceFn "extractPR<(a,b,c)>" $
          P_3 (extractPR as i# n#)
              (extractPR bs i# n#)
              (extractPR cs i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_3 as bs cs) n# is
    = traceFn "bpermutePR<(a,b,c)>" $
      P_3 (bpermutePR as n# is)
          (bpermutePR bs n# is)
          (bpermutePR cs n# is)

  {-# INLINE appPR #-}
  appPR (P_3 as1 bs1 cs1) (P_3 as2 bs2 cs2)
    = traceFn "appPR<(a,b,c)>" $
      P_3 (appPR as1 as2)
          (appPR bs1 bs2)
          (appPR cs1 cs2)

  {-# INLINE applPR #-}
  applPR is (P_3 as1 bs1 cs1) js (P_3 as2 bs2 cs2)
    = traceFn "applPR<(a,b,c)>" $
      P_3 (applPR is as1 js as2)
          (applPR is bs1 js bs2)
          (applPR is cs1 js cs2)

  {-# INLINE packPR #-}
  packPR (P_3 as bs cs) n# sel#
    = traceFn "packPR<(a,b,c)>" $
      P_3 (packPR as n# sel#)
          (packPR bs n# sel#)
          (packPR cs n# sel#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_3 as1 bs1 cs1)
                                  (P_3 as2 bs2 cs2)
    = traceFn "combine2PR<(a,b,c)>" $
      P_3 (combine2PR n# sel as1 as2)
          (combine2PR n# sel bs1 bs2)
          (combine2PR n# sel cs1 cs2)

  {-# INLINE fromListPR #-}
  fromListPR n# xs
    = P_3 (fromListPR n# as)
          (fromListPR n# bs)
          (fromListPR n# cs)
    where
      (as,bs,cs) = unzip3 xs

  {-# INLINE nfPR #-}
  nfPR (P_3 as bs cs)
    = nfPR as
      `seq` nfPR bs
      `seq` nfPR cs

zip3PA# :: PArray a -> PArray b -> PArray c -> PArray (a,b,c)
{-# INLINE_PA zip3PA# #-}
zip3PA# (PArray n# xs) (PArray _ ys) (PArray _ zs) = PArray n# (P_3 xs ys zs)


instance (PR a, PR b, PR c, PR d) => PR (a,b,c,d) where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR<(a,b,c,d)>" $
          P_4 emptyPR
              emptyPR
              emptyPR
              emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b,c,d)
    = traceFn "replicatePR<(a,b,c,d)>" $
      P_4 (replicatePR n# a)
          (replicatePR n# b)
          (replicatePR n# c)
          (replicatePR n# d)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_4 as bs cs ds)
    = traceFn "replicatelPR<(a,b,c,d)>" $
      P_4 (replicatelPR segd as)
          (replicatelPR segd bs)
          (replicatelPR segd cs)
          (replicatelPR segd ds)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_4 as bs cs ds)
    = traceFn "repeatPR<(a,b,c,d)>" $
      P_4 (repeatPR n# len# as)
          (repeatPR n# len# bs)
          (repeatPR n# len# cs)
          (repeatPR n# len# ds)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_4 as bs cs ds)
    = traceFn "repeatcPR<(a,b,c,d)>" $
      P_4 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)
          (repeatcPR n# ns segd cs)
          (repeatcPR n# ns segd ds)

  {-# INLINE indexPR #-}
  indexPR (P_4 as bs cs ds) i#
    = traceFn "indexPR<(a,b,c,d)>" $
      (indexPR as i#,
       indexPR bs i#,
       indexPR cs i#,
       indexPR ds i#)

  {-# INLINE extractPR #-}
  extractPR (P_4 as bs cs ds) i# n#
    = traceFn "extractPR<(a,b,c,d)>" $
        P_4 (extractPR as i# n#)
            (extractPR bs i# n#)
            (extractPR cs i# n#)
            (extractPR ds i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_4 as bs cs ds) n# is
    = traceFn "bpermutePR<(a,b,c,d)>" $
      P_4 (bpermutePR as n# is)
          (bpermutePR bs n# is)
          (bpermutePR cs n# is)
          (bpermutePR ds n# is)

  {-# INLINE appPR #-}
  appPR (P_4 as1 bs1 cs1 ds1) (P_4 as2 bs2 cs2 ds2)
    = traceFn "appPR<(a,b,c,d)>" $
      P_4 (appPR as1 as2)
          (appPR bs1 bs2)
          (appPR cs1 cs2)
          (appPR ds1 ds2)

  {-# INLINE applPR #-}
  applPR is (P_4 as1 bs1 cs1 ds1) js (P_4 as2 bs2 cs2 ds2)
    = traceFn "applPR<(a,b,c,d)>" $
      P_4 (applPR is as1 js as2)
          (applPR is bs1 js bs2)
          (applPR is cs1 js cs2)
          (applPR is ds1 js ds2)

  {-# INLINE packPR #-}
  packPR (P_4 as bs cs ds) n# sel#
    = traceFn "packPR<(a,b,c,d)>" $
      P_4 (packPR as n# sel#)
          (packPR bs n# sel#)
          (packPR cs n# sel#)
          (packPR ds n# sel#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_4 as1 bs1 cs1 ds1)
                                    (P_4 as2 bs2 cs2 ds2)
    = traceFn "combine2PR<(a,b,c,d)>" $
      P_4 (combine2PR n# sel as1 as2)
          (combine2PR n# sel bs1 bs2)
          (combine2PR n# sel cs1 cs2)
          (combine2PR n# sel ds1 ds2)

  {-# INLINE fromListPR #-}
  fromListPR n# xs
    = P_4 (fromListPR n# as)
          (fromListPR n# bs)
          (fromListPR n# cs)
          (fromListPR n# ds)
    where
      (as,bs,cs,ds) = L.unzip4 xs

  {-# INLINE nfPR #-}
  nfPR (P_4 as bs cs ds)
    = nfPR as
      `seq` nfPR bs
      `seq` nfPR cs
      `seq` nfPR ds

instance (PR a, PR b, PR c, PR d, PR e) => PR (a,b,c,d,e) where
  {-# INLINE emptyPR #-}
  emptyPR
    = traceFn "emptyPR<(a,b,c,d,e)>" $
    P_5 emptyPR
        emptyPR
        emptyPR
        emptyPR
        emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b,c,d,e)
    = traceFn "replicatePR<(a,b,c,d,e)>" $
    P_5 (replicatePR n# a)
        (replicatePR n# b)
        (replicatePR n# c)
        (replicatePR n# d)
        (replicatePR n# e)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_5 as bs cs ds es)
    = traceFn "replicatelPR<(a,b,c,d,e)>" $
    P_5 (replicatelPR segd as)
        (replicatelPR segd bs)
        (replicatelPR segd cs)
        (replicatelPR segd ds)
        (replicatelPR segd es)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_5 as bs cs ds es)
    = traceFn "repeatPR<(a,b,c,d,e)>" $
    P_5 (repeatPR n# len# as)
        (repeatPR n# len# bs)
        (repeatPR n# len# cs)
        (repeatPR n# len# ds)
        (repeatPR n# len# es)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_5 as bs cs ds es)
    = traceFn "repeatcPR<(a,b,c,d,e)>" $
    P_5 (repeatcPR n# ns segd as)
        (repeatcPR n# ns segd bs)
        (repeatcPR n# ns segd cs)
        (repeatcPR n# ns segd ds)
        (repeatcPR n# ns segd es)

  {-# INLINE indexPR #-}
  indexPR (P_5 as bs cs ds es) i#
    = traceFn "indexPR<(a,b,c,d,e)>" $
    (indexPR as i#,
     indexPR bs i#,
     indexPR cs i#,
     indexPR ds i#,
     indexPR es i#)

  {-# INLINE extractPR #-}
  extractPR (P_5 as bs cs ds es) i# n#
    = traceFn "extractPR<(a,b,c,d,e)>" $
      P_5 (extractPR as i# n#)
          (extractPR bs i# n#)
          (extractPR cs i# n#)
          (extractPR ds i# n#)
          (extractPR es i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_5 as bs cs ds es) n# is
    = traceFn "bpermutePR<(a,b,c,d,e)>" $
    P_5 (bpermutePR as n# is)
        (bpermutePR bs n# is)
        (bpermutePR cs n# is)
        (bpermutePR ds n# is)
        (bpermutePR es n# is)

  {-# INLINE appPR #-}
  appPR (P_5 as1 bs1 cs1 ds1 es1)
                              (P_5 as2 bs2 cs2 ds2 es2)
    = traceFn "appPR<(a,b,c,d,e)>" $
    P_5 (appPR as1 as2)
        (appPR bs1 bs2)
        (appPR cs1 cs2)
        (appPR ds1 ds2)
        (appPR es1 es2)

  {-# INLINE applPR #-}
  applPR is (P_5 as1 bs1 cs1 ds1 es1)
                               js (P_5 as2 bs2 cs2 ds2 es2)
    = traceFn "applPR<(a,b,c,d,e)>" $
    P_5 (applPR is as1 js as2)
        (applPR is bs1 js bs2)
        (applPR is cs1 js cs2)
        (applPR is ds1 js ds2)
        (applPR is es1 js es2)

  {-# INLINE packPR #-}
  packPR (P_5 as bs cs ds es) n# sel#
    = traceFn "packPR<(a,b,c,d,e)>" $
    P_5 (packPR as n# sel#)
        (packPR bs n# sel#)
        (packPR cs n# sel#)
        (packPR ds n# sel#)
        (packPR es n# sel#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_5 as1 bs1 cs1 ds1 es1)
                                          (P_5 as2 bs2 cs2 ds2 es2)
    = traceFn "combine2PR<(a,b,c,d,e)>" $
    P_5 (combine2PR n# sel as1 as2)
        (combine2PR n# sel bs1 bs2)
        (combine2PR n# sel cs1 cs2)
        (combine2PR n# sel ds1 ds2)
        (combine2PR n# sel es1 es2)

  {-# INLINE fromListPR #-}
  fromListPR n# xs
    = P_5 (fromListPR n# as)
          (fromListPR n# bs)
          (fromListPR n# cs)
          (fromListPR n# ds)
          (fromListPR n# es)
    where
      (as,bs,cs,ds,es) = L.unzip5 xs

  {-# INLINE nfPR #-}
  nfPR (P_5 as bs cs ds es)
    = nfPR as
      `seq` nfPR bs
      `seq` nfPR cs
      `seq` nfPR ds
      `seq` nfPR es

data Sum2 a b = Alt2_1 a | Alt2_2 b
data Sum3 a b c = Alt3_1 a | Alt3_2 b | Alt3_3 c

data instance PData (Sum2 a b)
  = PSum2 Sel2 (PData a) (PData b)

instance (PR a, PR b) => PR (Sum2 a b) where 
  {-# INLINE emptyPR #-}
  emptyPR
    = traceFn "emptyPR<Sum2>" $
    PSum2 (mkSel2 U.empty U.empty 0 0) emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (Alt2_1 x)
    = traceFn "replicatePR<Sum2>" $
      PSum2 (mkSel2 (U.replicate (I# n#) 0)
                    (U.enumFromStepLen 0 1 (I# n#))
                    (I# n#) 0)
            (replicatePR n# x)
            emptyPR
  replicatePR n# (Alt2_2 x)
    = traceFn "replicatePR<Sum2>" $
      PSum2 (mkSel2 (U.replicate (I# n#) 1)
                    (U.enumFromStepLen 0 1 (I# n#))
                    0 (I# n#))
            emptyPR
            (replicatePR n# x)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PSum2 sel as bs)
    = traceFn "replicatelPR<Sum2>" $
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

      as'       = replicatelPR asegd as
      bs'       = replicatelPR bsegd bs

  {-# INLINE repeatPR #-}
  repeatPR m# n# (PSum2 sel as bs)
    = traceFn "repeatPR<Sum2>" $
      PSum2 sel' as' bs'
    where
      sel' = tagsToSel2
           . U.repeat (I# m#) (I# n#)
           $ tagsSel2 sel

      as'  = repeatPR m# (elementsSel2_0# sel) as
      bs'  = repeatPR n# (elementsSel2_1# sel) bs

  {-# INLINE indexPR #-}
  indexPR (PSum2 sel as bs) i#
    = traceFn "indexPR<Sum2>" $
    case indicesSel2 sel U.!: I# i# of
      I# k# -> case tagsSel2 sel U.!: I# i# of
                 0 -> Alt2_1 (indexPR as k#)
                 _ -> Alt2_2 (indexPR bs k#)

  {-# INLINE appPR #-}
  appPR (PSum2 sel1 as1 bs1)
                     (PSum2 sel2 as2 bs2)
    = traceFn "appPR<Sum2>" $           
      PSum2 sel (appPR as1 as2)
                (appPR bs1 bs2)
    where
      sel = tagsToSel2
          $ tagsSel2 sel1 U.+:+ tagsSel2 sel2

  {-# INLINE packPR #-}
  packPR (PSum2 sel as bs) m# flags
    = traceFn "packPR<Sum2>" $
      PSum2 sel' as' bs'
    where
      tags   = tagsSel2 sel
      tags'  = U.pack tags flags
      sel'   = tagsToSel2 tags'

      aFlags = U.pack flags (U.pick tags 0)
      bFlags = U.pack flags (U.pick tags 1)

      as'    = packPR as (elementsSel2_0# sel') aFlags
      bs'    = packPR bs (elementsSel2_1# sel') bFlags

      _dummy :: Int#
      _dummy = m#

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PSum2 sel1 as1 bs1)
                                 (PSum2 sel2 as2 bs2)
    = traceFn "combine2PR<Sum2>" $
      PSum2 sel' as bs
    where
      tags  = tagsSel2 sel
      tags' = U.combine (U.pick tags 0) (tagsSel2 sel1) (tagsSel2 sel2)
      sel'  = tagsToSel2 tags'

      asel = tagsToSel2 (U.pack tags (U.pick tags' 0))
      bsel = tagsToSel2 (U.pack tags (U.pick tags' 1))

      as   = combine2PR (elementsSel2_0# sel') asel as1 as2
      bs   = combine2PR (elementsSel2_1# sel') bsel bs1 bs2

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

{-
{-# INLINE nested_lengthPA #-}
nested_lengthPA xss = traceFn "nested_lengthPA\n" $
                I# (lengthPR_PArray xss)
-}

instance PR a => PR (PArray a) where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR<PArray>" $
          PNested (U.mkSegd U.empty U.empty 0) emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (PArray m# xs)
    = traceFn "replicatePR<PArray>" $
    PNested (U.mkSegd (U.replicate (I# n#) (I# m#))
                      (U.enumFromStepLen 0 (I# m#) (I# n#))
                      (I# n# * I# m#))
            (repeatPR n# m# xs)

  {-# INLINE indexPR #-}
  indexPR (PNested segd xs) i#
    = case U.lengthsSegd segd U.!: I# i# of { I# n# ->
      case U.indicesSegd segd U.!: I# i# of { I# k# ->
      PArray n# (extractPR xs k# n#) }}

  {-# INLINE extractPR #-}
  extractPR (PNested segd xs) i# n#
    = case U.indicesSegd segd U.!: I# i# of
        I# k# -> PNested segd' (extractPR xs k# (elementsSegd# segd'))
    where
      segd' = U.lengthsToSegd
            $ U.extract (U.lengthsSegd segd) (I# i#) (I# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (PNested segd xs) n# is
    = traceFn "bpermutePR<PArray>" $
      PNested segd' (bpermutePR xs (elementsSegd# segd') flat_is)
    where
      segd'   = U.lengthsToSegd
              $ U.bpermute (U.lengthsSegd segd) is

      starts  = U.indicesSegd segd'
      lens    = U.lengthsSegd segd'
      ends    = U.zipWith (\x y -> x + y - 1) starts lens
      flat_is = U.enumFromToEach (U.elementsSegd segd') (U.zip starts ends)

  {-# INLINE appPR #-}
  appPR (PNested xsegd xs) (PNested ysegd ys)
    = traceFn "appPR<PArray>" $             
      PNested (U.lengthsToSegd (U.lengthsSegd xsegd U.+:+ U.lengthsSegd ysegd))
              (appPR xs ys)

  {-# INLINE applPR #-}
  applPR segd1 (PNested xsegd xs) segd2 (PNested ysegd ys)
    = PNested segd (applPR xsegd' xs ysegd' ys)
    where
      segd = U.lengthsToSegd
           $ U.append_s segd1 (U.lengthsSegd xsegd) segd2 (U.lengthsSegd ysegd)

      xsegd' = U.lengthsToSegd
             $ U.sum_s segd1 (U.lengthsSegd xsegd)
      ysegd' = U.lengthsToSegd
             $ U.sum_s segd2 (U.lengthsSegd ysegd)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (PNested segd xs)
    = traceFn "repeatPR<PArray>" $
    PNested segd' (repeatPR n# (elementsSegd# segd) xs)
    where
      segd' = U.lengthsToSegd (U.repeat (I# n#) (I# len#) (U.lengthsSegd segd))

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PNested xsegd xs)
    = traceFn "replicatelPR<PArray>" $
    PNested xsegd' xs'
    where
      xsegd' = U.lengthsToSegd
             $ U.replicate_s segd (U.lengthsSegd xsegd)

      xs'    = repeatcPR (elementsSegd# xsegd')
                            (U.lengthsSegd segd)
                            xsegd xs

  {-# INLINE packPR #-}
  packPR (PNested segd xs) n# flags
    = traceFn "packPR<PArray>" $
    PNested segd' xs'
    where
      segd' = U.lengthsToSegd
            $ U.pack (U.lengthsSegd segd) flags

      xs'   = packPR xs (elementsSegd# segd') (U.replicate_s segd flags)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PNested xsegd xs) (PNested ysegd ys)
    = traceFn "combine2PR<PArray>" $
    PNested segd xys
    where
      tags = tagsSel2 sel

      segd = U.lengthsToSegd
           $ U.combine (U.pick tags 0) (U.lengthsSegd xsegd) (U.lengthsSegd ysegd)

      sel' = tagsToSel2
           $ U.replicate_s segd tags

      xys  = combine2PR (elementsSegd# segd) sel' xs ys

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

