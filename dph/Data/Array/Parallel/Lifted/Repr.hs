{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Repr (
  PArray(..),
  Void, void,
  Wrap(..),
  Enumeration(..),
  Sum2(..), Sum3(..), 

  dPA_Void,
  dPR_Void, dPR_Unit, dPR_Wrap,
  dPR_Enumeration,
  dPR_2, dPR_3, dPR_4, dPR_5, zipPA#, unzipPA#, zip3PA#,
  dPR_Sum2, dPR_Sum3,

  dPR_PArray, nested_lengthPA, concatPA#,
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed

import Data.Array.Parallel.Base ((:*:)(..), fromBool)

import qualified Data.List as L
import GHC.Exts  (Int#, Int(..), (+#), (-#), (*#))
import Debug.Trace



data Void

traceFn   = flip const -- trace
traceArgs = flip  const -- trace 

void :: Void
void = error "Data.Array.Parallel.void"

data instance PArray Void = PVoid Int#

dPR_Void :: PR Void
{-# INLINE dPR_Void #-}
dPR_Void = PR {
             lengthPR     = lengthPR_Void
           , emptyPR      = emptyPR_Void
           , replicatePR  = replicatePR_Void
           , replicatelPR = replicatelPR_Void
           , repeatPR     = repeatPR_Void
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

{-# INLINE lengthPR_Void #-}
lengthPR_Void (PVoid n#) = 
              n#

{-# INLINE emptyPR_Void #-}
emptyPR_Void = traceFn "emptyPR_Void" $
             PVoid 0#

{-# INLINE replicatePR_Void #-}
replicatePR_Void n# _ = traceFn "replicatePR_Void" $
                 PVoid n#

{-# INLINE replicatelPR_Void #-}
replicatelPR_Void n# _ _ = traceFn "replicatelPR_Void" $
                  PVoid n#

{-# INLINE repeatPR_Void #-}
repeatPR_Void n# (PVoid m#) = traceFn "repeatPR_Void" $
              PVoid (n# *# m#)

indexPR_Void :: PArray Void -> Int# -> Void
{-# INLINE indexPR_Void #-}
indexPR_Void (PVoid n#) i# = traceFn "indexPR_Void" $
             void

extractPR_Void :: PArray Void -> Int# -> Int# -> PArray Void
{-# INLINE extractPR_Void #-}
extractPR_Void (PVoid _) i# n# = traceFn "extractPR_Void" $
             PVoid n#

{-# INLINE bpermutePR_Void #-}
bpermutePR_Void n# (PVoid _) _ = traceFn "bpermutePR_Void" $
                PVoid n#

{-# INLINE appPR_Void #-}
appPR_Void (PVoid m#) (PVoid n#) = traceFn "appPR_Void" $
           PVoid (m# +# n#)

{-# INLINE applPR_Void #-}
applPR_Void _ (PVoid m#) _ (PVoid n#) = traceFn "applPR_Void" $
            PVoid (m# +# n#)

{-# INLINE packPR_Void #-}
packPR_Void (PVoid _) n# _ = traceFn "packPR_Void" $
            PVoid n#

{-# INLINE combine2PR_Void #-}
combine2PR_Void n# _ _ (PVoid _) (PVoid _) = traceFn "combine2PR_Void" $
                PVoid n#

{-# INLINE fromListPR_Void #-}
fromListPR_Void n# _ = PVoid n#

{-# INLINE nfPR_Void #-}
nfPR_Void (PVoid _) = ()

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

data instance PArray () = PUnit Int# ()

dPR_Unit :: PR ()
{-# INLINE dPR_Unit #-}
dPR_Unit = PR {
             lengthPR     = lengthPR_Unit
           , emptyPR      = emptyPR_Unit
           , replicatePR  = replicatePR_Unit
           , replicatelPR = replicatelPR_Unit
           , repeatPR     = repeatPR_Unit
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
         

{-# INLINE lengthPR_Unit #-}
lengthPR_Unit (PUnit n# _) = 
              n#

{-# INLINE emptyPR_Unit #-}
emptyPR_Unit = traceFn "emptyPR_Unit" $
             PUnit 0# ()

{-# INLINE replicatePR_Unit #-}
replicatePR_Unit n# u = 
  traceFn "replicatePR_Unit" $
  traceArgs ("replicatePR_Unit len = " ++ show (I# n#)) $
                 PUnit n# u

{-# INLINE replicatelPR_Unit #-}
replicatelPR_Unit n# _ (PUnit _ u) = 
  traceFn "replicatelPR_Unit" 
  traceArgs ("replicatelPR_Unit args len = " ++ (show (I# n#)))$
  PUnit n# u

{-# INLINE repeatPR_Unit #-}
repeatPR_Unit n# (PUnit m# u) = traceFn "repeatPR_Unit" $
              PUnit (n# *# m#) u

indexPR_Unit :: PArray () -> Int# -> ()
{-# INLINE indexPR_Unit #-}
indexPR_Unit (PUnit n# u) i# = traceFn "indexPR_Unit" $
             u

extractPR_Unit :: PArray () -> Int# -> Int# -> PArray ()
{-# INLINE extractPR_Unit #-}
extractPR_Unit (PUnit _ u) _ n# = traceFn "extractPR_Unit" $
             PUnit n# u

{-# INLINE bpermutePR_Unit #-}
bpermutePR_Unit n# (PUnit _ u) _ = traceFn "bpermutePR_Unit" $
                PUnit n# u

{-# INLINE appPR_Unit #-}
appPR_Unit (PUnit m# u) (PUnit n# v) = traceFn "appPR_Unit" $
           PUnit (m# +# n#) (u `seq` v)

{-# INLINE applPR_Unit #-}
applPR_Unit _ (PUnit m# u) _ (PUnit n# v) = traceFn "applPR_Unit" $
            PUnit (m# +# n#) (u `seq` v)

{-# INLINE packPR_Unit #-}
packPR_Unit (PUnit _ u) n# _ = traceFn "packPR_Unit" $
            PUnit n# u

{-# INLINE combine2PR_Unit #-}
combine2PR_Unit n# _ _ (PUnit _ u1) (PUnit _ u2) = traceFn "combine2PR_Unit" $
  PUnit n# (u1 `seq` u2)

{-# INLINE fromListPR_Unit #-}
fromListPR_Unit n# xs = PUnit n# (foldr seq () xs)

{-# INLINE nfPR_Unit #-}
nfPR_Unit (PUnit _ u) = u

data Wrap a = Wrap a

data instance PArray (Wrap a) = PWrap Int# (PArray a)

dPR_Wrap :: PR a -> PR (Wrap a)
{-# INLINE dPR_Wrap #-}
dPR_Wrap pr = PR {
              lengthPR     = lengthPR_Wrap
            , emptyPR      = emptyPR_Wrap pr
            , replicatePR  = replicatePR_Wrap pr
            , replicatelPR = replicatelPR_Wrap pr
            , repeatPR     = repeatPR_Wrap pr
            , indexPR      = indexPR_Wrap pr
            , extractPR    = extractPR_Wrap pr
            , bpermutePR   = bpermutePR_Wrap pr
            , appPR        = appPR_Wrap pr
            , applPR       = applPR_Wrap pr
            , packPR       = packPR_Wrap pr
            , combine2PR   = combine2PR_Wrap pr
            }

{-# INLINE lengthPR_Wrap #-}
lengthPR_Wrap (PWrap n# _) = 
              n#

{-# INLINE emptyPR_Wrap #-}
emptyPR_Wrap pr = traceFn "emptyPR_Wrap" $
             PWrap 0# (emptyPR pr)

{-# INLINE replicatePR_Wrap #-}
replicatePR_Wrap pr n# ~(Wrap x) = traceFn "replicatePR_Wrap" $
                 PWrap n# (replicatePR pr n# x)

{-# INLINE replicatelPR_Wrap #-}
replicatelPR_Wrap pr n# ns (PWrap _ xs) = traceFn "replicatelPR_Wrap" $
                  PWrap n# (replicatelPR pr n# ns xs)

{-# INLINE repeatPR_Wrap #-}
repeatPR_Wrap pr n# (PWrap m# xs) = traceFn "repeatPR_Wrap" $
              PWrap (n# *# m#) (repeatPR pr n# xs)

{-# INLINE indexPR_Wrap #-}
indexPR_Wrap pr (PWrap n# xs) i# = traceFn "indexPR_Wrap" $
             Wrap (indexPR pr xs i#)

{-# INLINE extractPR_Wrap #-}
extractPR_Wrap pr (PWrap _ xs) i# n# = traceFn "extractPR_Wrap" $
             PWrap n# (extractPR pr xs i# n#)

{-# INLINE bpermutePR_Wrap #-}
bpermutePR_Wrap pr n# (PWrap _ xs) is = traceFn "bpermutePR_Wrap" $
                PWrap n# (bpermutePR pr n# xs is)

{-# INLINE appPR_Wrap #-}
appPR_Wrap pr (PWrap m# xs) (PWrap n# ys) = traceFn "appPR_Wrap" $
           PWrap (m# +# n#) (appPR pr xs ys)

{-# INLINE applPR_Wrap #-}
applPR_Wrap pr is (PWrap m# xs) js (PWrap n# ys) = traceFn "applPR_Wrap" $
            PWrap (m# +# n#) (applPR pr is xs js ys)

{-# INLINE packPR_Wrap #-}
packPR_Wrap pr (PWrap _ xs) n# sel# = traceFn "packPR_Wrap" $
            PWrap n# (packPR pr xs n# sel#)

combine2PR_Wrap:: PR a -> Int# -> PArray_Int# -> PArray_Int#
                              -> PArray (Wrap a) -> PArray (Wrap a) -> PArray (Wrap a)
combine2PR_Wrap _ _ _ _ _ = traceFn "combine2PR_Wrap" $
                error "combine2PR_Wrap nyi"

data Enumeration = Enumeration Int#

data instance PArray Enumeration = PEnum Int# PArray_Int# PArray_Int#

dPR_Enumeration :: PR Enumeration
{-# INLINE dPR_Enumeration #-}
dPR_Enumeration = PR {
                    lengthPR    = lengthPR_Enumeration
                  , emptyPR     = emptyPR_Enumeration
                  , replicatePR = replicatePR_Enumeration
                  }

{-# INLINE lengthPR_Enumeration #-}
lengthPR_Enumeration (PEnum n# _ _) = n#

{-# INLINE emptyPR_Enumeration #-}
emptyPR_Enumeration = traceFn "emptyPR_Enumeration" $
                    PEnum 0# emptyPA_Int# emptyPA_Int#

{-# INLINE replicatePR_Enumeration #-}
replicatePR_Enumeration n# enum
  = traceFn "replicatePR_Enumeration" $
      PEnum n# (replicatePA_Int# n# (case enum of { Enumeration i# -> i# }))
             (upToPA_Int# n#)

data instance PArray (a,b)
  = P_2 Int# (PArray a)
             (PArray b)

data instance PArray (a,b,c)
  = P_3 Int# (PArray a)
             (PArray b)
             (PArray c)

data instance PArray (a,b,c,d)
  = P_4 Int# (PArray a)
             (PArray b)
             (PArray c)
             (PArray d)

data instance PArray (a,b,c,d,e)
  = P_5 Int# (PArray a)
             (PArray b)
             (PArray c)
             (PArray d)
             (PArray e)

dPR_2 :: PR a -> PR b -> PR (a,b)
{-# INLINE dPR_2 #-}
dPR_2 pra prb
  = PR {
      lengthPR     = lengthPR_2
    , emptyPR      = emptyPR_2 pra prb
    , replicatePR  = replicatePR_2 pra prb
    , replicatelPR = replicatelPR_2 pra prb
    , repeatPR     = repeatPR_2 pra prb
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

{-# INLINE lengthPR_2 #-}
lengthPR_2 (P_2 n# _ _) = n#

{-# INLINE emptyPR_2 #-}
emptyPR_2 pra prb = traceFn "emptyPR_2" $
          P_2 0# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_2 #-}
replicatePR_2 pra prb n# ~(a,b) = 
  traceFn "replicatePR_2" $
  traceArgs ("replicatePR_2 args len = "  ++ (show (I# n#))) $
  P_2 n# (replicatePR pra n# a)
           (replicatePR prb n# b)

{-# INLINE replicatelPR_2 #-}
replicatelPR_2 pra prb n# ns (P_2 _ as bs)
  = traceFn "replicatelPR_2" $
  P_2 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs) 

{-# INLINE repeatPR_2 #-}
repeatPR_2 pra prb n# (P_2 m# as bs)
  = traceFn "repeatPR_2" $
  P_2 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)

{-# INLINE indexPR_2 #-}
indexPR_2 pra prb (P_2 _ as bs) i# = traceFn "indexPR_2" $
          (indexPR pra as i#, indexPR prb bs i#)

{-# INLINE extractPR_2 #-}
extractPR_2 pra prb (P_2 _ as bs) i# n# = traceFn "extractPR_2" $
          P_2 n# (extractPR pra as i# n#)
                 (extractPR prb bs i# n#)

{-# INLINE bpermutePR_2 #-}
bpermutePR_2 pra prb n# (P_2 _ as bs) is
  = traceFn "bpermutePR_2" $
  P_2 n# (bpermutePR pra n# as is) (bpermutePR prb n# bs is)

{-# INLINE appPR_2 #-}
appPR_2 pra prb (P_2 m# as1 bs1) (P_2 n# as2 bs2)
  = P_2 (m# +# n#) (appPR pra as1 as2) (appPR prb bs1 bs2)

{-# INLINE applPR_2 #-}
applPR_2 pra prb is (P_2 m# as1 bs1) js (P_2 n# as2 bs2)
  = traceFn "applPR_2" $
  P_2 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)

{-# INLINE packPR_2 #-}
packPR_2 pra prb (P_2 _ as bs) n# sel# = traceFn "packPR_2" $
         P_2 n# (packPR pra as n# sel#)
                                                (packPR prb bs n# sel#)

{-# INLINE combine2PR_2 #-}
combine2PR_2 pra prb n# sel# is# (P_2 _ as1 bs1) (P_2 _ as2 bs2)
  = traceFn "combine2PR_2" $
       P_2 n# (combine2PR pra n# sel# is# as1 as2)
              (combine2PR prb n# sel# is# bs1 bs2)

{-# INLINE fromListPR_2 #-}
fromListPR_2 pra prb n# xs
  = P_2 n# (fromListPR pra n# as)
           (fromListPR prb n# bs)
  where
    (as,bs) = unzip xs

{-# INLINE nfPR_2 #-}
nfPR_2 pra prb (P_2 _ as bs)
  = nfPR pra as `seq` nfPR prb bs

zipPA# :: PA a -> PA b -> PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA# #-}
zipPA# pa pb xs ys = 
  traceFn "zipPA" $ 
  traceArgs  ("zipPA args len1:" ++ show (I# (lengthPA# pa xs)) ++
               "\nlen2:" ++ show (I# (lengthPA# pb ys))
    ) $
       P_2 (lengthPA# pa xs) xs ys

unzipPA# :: PA a -> PA b  -> PArray (a,b) -> (PArray a,  PArray b)
{-# INLINE_PA unzipPA# #-}
unzipPA# pa pb (P_2 n xs ys)  = traceFn "unzipPA" $
         (xs, ys)

dPR_3 :: PR a -> PR b -> PR c -> PR (a,b,c)
{-# INLINE dPR_3 #-}
dPR_3 pra prb prc
  = PR {
      lengthPR     = lengthPR_3
    , emptyPR      = emptyPR_3 pra prb prc
    , replicatePR  = replicatePR_3 pra prb prc
    , replicatelPR = replicatelPR_3 pra prb prc
    , repeatPR     = repeatPR_3 pra prb prc
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

{-# INLINE lengthPR_3 #-}
lengthPR_3 (P_3 n# _ _ _) = n#

{-# INLINE emptyPR_3 #-}
emptyPR_3 pra prb prc = traceFn "emptyPR_3" $
          P_3 0# (emptyPR pra) (emptyPR prb) (emptyPR prc)

{-# INLINE replicatePR_3 #-}
replicatePR_3 pra prb prc n# ~(a,b,c)
  = traceFn "replicatePR_3" $
  P_3 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)

{-# INLINE replicatelPR_3 #-}
replicatelPR_3 pra prb prc n# ns (P_3 _ as bs cs)
  = traceFn "replicatelPR_3" $
  P_3 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)

{-# INLINE repeatPR_3 #-}
repeatPR_3 pra prb prc n# (P_3 m# as bs cs)
  = traceFn "repeatPR_3" $
  P_3 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)

{-# INLINE indexPR_3 #-}
indexPR_3 pra prb prc (P_3 n# as bs cs) i#
  = traceFn "indexPR_3" $
  (indexPR pra as i#, indexPR prb bs i#, indexPR prc cs i#)

{-# INLINE extractPR_3 #-}
extractPR_3 pra prb prc (P_3 _ as bs cs) i# n# = traceFn "extractPR_3" $
          P_3 n# (extractPR pra as i# n#)
                 (extractPR prb bs i# n#)
                 (extractPR prc cs i# n#)

{-# INLINE bpermutePR_3 #-}
bpermutePR_3 pra prb prc n# (P_3 _ as bs cs) is
  = traceFn "bpermutePR_3" $
  P_3 n# (bpermutePR pra n# as is)
         (bpermutePR prb n# bs is)
         (bpermutePR prc n# cs is)

{-# INLINE appPR_3 #-}
appPR_3 pra prb prc (P_3 m# as1 bs1 cs1) (P_3 n# as2 bs2 cs2)
  = traceFn "appPR_3" $
  P_3 (m# +# n#) (appPR pra as1 as2) (appPR prb bs1 bs2) (appPR prc cs1 cs2)

{-# INLINE applPR_3 #-}
applPR_3 pra prb prc is (P_3 m# as1 bs1 cs1) js (P_3 n# as2 bs2 cs2)
  = traceFn "applPR_3" $
  P_3 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)
                   (applPR prc is cs1 js cs2)

{-# INLINE packPR_3 #-}
packPR_3 pra prb prc (P_3 _ as bs cs) n# sel#
  = traceFn "packPR_3" $
  P_3 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)

{-# INLINE combine2PR_3 #-}
combine2PR_3 pra prb prc n# sel# is# (P_3 _ as1 bs1 cs1)
                                     (P_3 _ as2 bs2 cs2)
  = traceFn "combine2PR_3" $
  P_3 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)

{-# INLINE fromListPR_3 #-}
fromListPR_3 pra prb prc n# xs
  = P_3 n# (fromListPR pra n# as)
           (fromListPR prb n# bs)
           (fromListPR prc n# cs)
  where
    (as,bs,cs) = unzip3 xs

{-# INLINE nfPR_3 #-}
nfPR_3 pra prb prc (P_3 _ as bs cs)
  = nfPR pra as
    `seq` nfPR prb bs
    `seq` nfPR prc cs

zip3PA# :: PA a -> PA b -> PA c
        -> PArray a -> PArray b -> PArray c -> PArray (a,b,c)
{-# INLINE_PA zip3PA# #-}
zip3PA# pa pb pc xs ys zs = traceFn "zip3PA" $
        P_3 (lengthPA# pa xs) xs ys zs

dPR_4 :: PR a -> PR b -> PR c -> PR d -> PR (a,b,c,d)
{-# INLINE dPR_4 #-}
dPR_4 pra prb prc prd
  = PR {
      lengthPR     = lengthPR_4
    , emptyPR      = emptyPR_4 pra prb prc prd
    , replicatePR  = replicatePR_4 pra prb prc prd
    , replicatelPR = replicatelPR_4 pra prb prc prd
    , repeatPR     = repeatPR_4 pra prb prc prd
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

{-# INLINE lengthPR_4 #-}
lengthPR_4 (P_4 n# _ _ _ _) = n#

{-# INLINE emptyPR_4 #-}
emptyPR_4 pra prb prc prd = traceFn "emptyPR_4" $
          P_4 0# (emptyPR pra)
                                   (emptyPR prb)
                                   (emptyPR prc)
                                   (emptyPR prd)

{-# INLINE replicatePR_4 #-}
replicatePR_4 pra prb prc prd n# ~(a,b,c,d)
  = traceFn "replicatePR_4" $
  P_4 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)
           (replicatePR prd n# d)

{-# INLINE replicatelPR_4 #-}
replicatelPR_4 pra prb prc prd n# ns (P_4 _ as bs cs ds)
  = traceFn "replicatelPR_4" $
  P_4 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)
           (replicatelPR prd n# ns ds)

{-# INLINE repeatPR_4 #-}
repeatPR_4 pra prb prc prd n# (P_4 m# as bs cs ds)
  = traceFn "repeatPR_4" $
  P_4 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)
                   (repeatPR prd n# ds)

{-# INLINE indexPR_4 #-}
indexPR_4 pra prb prc prd (P_4 n# as bs cs ds) i#
  = traceFn "indexPR_4" $
  (indexPR pra as i#,
     indexPR prb bs i#,
     indexPR prc cs i#,
     indexPR prd ds i#)

{-# INLINE extractPR_4 #-}
extractPR_4 pra prb prc prd (P_4 _ as bs cs ds) i# n#
  = traceFn "extractPR_4" $
    P_4 n# (extractPR pra as i# n#)
           (extractPR prb bs i# n#)
           (extractPR prc cs i# n#)
           (extractPR prd ds i# n#)

{-# INLINE bpermutePR_4 #-}
bpermutePR_4 pra prb prc prd n# (P_4 _ as bs cs ds) is
  = traceFn "bpermutePR_4" $
  P_4 n# (bpermutePR pra n# as is)
         (bpermutePR prb n# bs is)
         (bpermutePR prc n# cs is)
         (bpermutePR prd n# ds is)

{-# INLINE appPR_4 #-}
appPR_4 pra prb prc prd (P_4 m# as1 bs1 cs1 ds1) (P_4 n# as2 bs2 cs2 ds2)
  = traceFn "appPR_4" $
  P_4 (m# +# n#) (appPR pra as1 as2)
                   (appPR prb bs1 bs2)
                   (appPR prc cs1 cs2)
                   (appPR prd ds1 ds2)

{-# INLINE applPR_4 #-}
applPR_4 pra prb prc prd is (P_4 m# as1 bs1 cs1 ds1) js (P_4 n# as2 bs2 cs2 ds2)
  = traceFn "applPR_4" $
  P_4 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)
                   (applPR prc is cs1 js cs2)
                   (applPR prd is ds1 js ds2)

{-# INLINE packPR_4 #-}
packPR_4 pra prb prc prd (P_4 _ as bs cs ds) n# sel#
  = traceFn "packPR_4" $
  P_4 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)
           (packPR prd ds n# sel#)

{-# INLINE combine2PR_4 #-}
combine2PR_4 pra prb prc prd n# sel# is# (P_4 _ as1 bs1 cs1 ds1)
                                         (P_4 _ as2 bs2 cs2 ds2)
  = traceFn "combine2PR_4" $
  P_4 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)
           (combine2PR prd n# sel# is# ds1 ds2)

{-# INLINE fromListPR_4 #-}
fromListPR_4 pra prb prc prd n# xs
  = P_4 n# (fromListPR pra n# as)
           (fromListPR prb n# bs)
           (fromListPR prc n# cs)
           (fromListPR prd n# ds)
  where
    (as,bs,cs,ds) = L.unzip4 xs

{-# INLINE nfPR_4 #-}
nfPR_4 pra prb prc prd (P_4 _ as bs cs ds)
  = nfPR pra as
    `seq` nfPR prb bs
    `seq` nfPR prc cs
    `seq` nfPR prd ds

dPR_5 :: PR a -> PR b -> PR c -> PR d -> PR e -> PR (a,b,c,d,e)
{-# INLINE dPR_5 #-}
dPR_5 pra prb prc prd pre
  = PR {
      lengthPR     = lengthPR_5
    , emptyPR      = emptyPR_5 pra prb prc prd pre
    , replicatePR  = replicatePR_5 pra prb prc prd pre
    , replicatelPR = replicatelPR_5 pra prb prc prd pre
    , repeatPR     = repeatPR_5 pra prb prc prd pre
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

{-# INLINE lengthPR_5 #-}
lengthPR_5 (P_5 n# _ _ _ _ _) = n#

{-# INLINE emptyPR_5 #-}
emptyPR_5 pra prb prc prd pre
  = traceFn "emptyPR_5" $
  P_5 0# (emptyPR pra)
           (emptyPR prb)
           (emptyPR prc)
           (emptyPR prd)
           (emptyPR pre)

{-# INLINE replicatePR_5 #-}
replicatePR_5 pra prb prc prd pre n# ~(a,b,c,d,e)
  = traceFn "replicatePR_5" $
  P_5 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)
           (replicatePR prd n# d)
           (replicatePR pre n# e)

{-# INLINE replicatelPR_5 #-}
replicatelPR_5 pra prb prc prd pre n# ns (P_5 _ as bs cs ds es)
  = traceFn "replicatelPR_5" $
  P_5 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)
           (replicatelPR prd n# ns ds)
           (replicatelPR pre n# ns es)

{-# INLINE repeatPR_5 #-}
repeatPR_5 pra prb prc prd pre n# (P_5 m# as bs cs ds es)
  = traceFn "repeatPR_5" $
  P_5 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)
                   (repeatPR prd n# ds)
                   (repeatPR pre n# es)

{-# INLINE indexPR_5 #-}
indexPR_5 pra prb prc prd pre (P_5 n# as bs cs ds es) i#
  = traceFn "indexPR_5" $
  (indexPR pra as i#,
     indexPR prb bs i#,
     indexPR prc cs i#,
     indexPR prd ds i#,
     indexPR pre es i#)

{-# INLINE extractPR_5 #-}
extractPR_5 pra prb prc prd pre (P_5 _ as bs cs ds es) i# n#
  = traceFn "extractPR_5" $
    P_5 n# (extractPR pra as i# n#)
           (extractPR prb bs i# n#)
           (extractPR prc cs i# n#)
           (extractPR prd ds i# n#)
           (extractPR pre es i# n#)

{-# INLINE bpermutePR_5 #-}
bpermutePR_5 pra prb prc prd pre n# (P_5 _ as bs cs ds es) is
  = traceFn "bpermutePR_5" $
  P_5 n# (bpermutePR pra n# as is)
         (bpermutePR prb n# bs is)
         (bpermutePR prc n# cs is)
         (bpermutePR prd n# ds is)
         (bpermutePR pre n# es is)

{-# INLINE appPR_5 #-}
appPR_5 pra prb prc prd pre (P_5 m# as1 bs1 cs1 ds1 es1)
                            (P_5 n# as2 bs2 cs2 ds2 es2)
  = traceFn "appPR_5" $
  P_5 (m# +# n#) (appPR pra as1 as2)
                   (appPR prb bs1 bs2)
                   (appPR prc cs1 cs2)
                   (appPR prd ds1 ds2)
                   (appPR pre es1 es2)

{-# INLINE applPR_5 #-}
applPR_5 pra prb prc prd pre is (P_5 m# as1 bs1 cs1 ds1 es1)
                             js (P_5 n# as2 bs2 cs2 ds2 es2)
  = traceFn "applPR_5" $
  P_5 (m# +# n#) (applPR pra is as1 js as2)
                   (applPR prb is bs1 js bs2)
                   (applPR prc is cs1 js cs2)
                   (applPR prd is ds1 js ds2)
                   (applPR pre is es1 js es2)

{-# INLINE packPR_5 #-}
packPR_5 pra prb prc prd pre (P_5 _ as bs cs ds es) n# sel#
  = traceFn "packPR_5" $
  P_5 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)
           (packPR prd ds n# sel#)
           (packPR pre es n# sel#)

{-# INLINE combine2PR_5 #-}
combine2PR_5 pra prb prc prd pre n# sel# is# (P_5 _ as1 bs1 cs1 ds1 es1)
                                             (P_5 _ as2 bs2 cs2 ds2 es2)
  = traceFn "combine2PR_5" $
  P_5 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)
           (combine2PR prd n# sel# is# ds1 ds2)
           (combine2PR pre n# sel# is# es1 es2)

{-# INLINE fromListPR_5 #-}
fromListPR_5 pra prb prc prd pre n# xs
  = P_5 n# (fromListPR pra n# as)
           (fromListPR prb n# bs)
           (fromListPR prc n# cs)
           (fromListPR prd n# ds)
           (fromListPR pre n# es)
  where
    (as,bs,cs,ds,es) = L.unzip5 xs

{-# INLINE nfPR_5 #-}
nfPR_5 pra prb prc prd pre (P_5 _ as bs cs ds es)
  = nfPR pra as
    `seq` nfPR prb bs
    `seq` nfPR prc cs
    `seq` nfPR prd ds
    `seq` nfPR pre es

data Sum2 a b = Alt2_1 a | Alt2_2 b
data Sum3 a b c = Alt3_1 a | Alt3_2 b | Alt3_3 c

data instance PArray (Sum2 a b)
  = PSum2 Int# PArray_Int# PArray_Int# (PArray a)
                                      (PArray b)

data instance PArray (Sum3 a b c)
  = PSum3 Int# PArray_Int# PArray_Int# (PArray a)
                                       (PArray b)
                                       (PArray c)

dPR_Sum2 :: PR a -> PR b -> PR (Sum2 a b)
{-# INLINE dPR_Sum2 #-}
dPR_Sum2 pra prb = PR {
                     lengthPR     = lengthPR_Sum2
                   , emptyPR      = emptyPR_Sum2 pra prb
                   , replicatePR  = replicatePR_Sum2 pra prb
                   , replicatelPR = replicatelPR_Sum2 pra prb
                   , repeatPR     = repeatPR_Sum2 pra prb
                   , indexPR      = indexPR_Sum2 pra prb
                   , bpermutePR   = bpermutePR_Sum2 pra prb 
                   , appPR        = appPR_Sum2 pra prb 
                   , applPR       = applPR_Sum2 pra prb 
                   , packPR       = packPR_Sum2 pra prb 
                   , combine2PR   = combine2PR_Sum2 pra prb 
                   }
 
{-# INLINE lengthPR_Sum2 #-}
lengthPR_Sum2 (PSum2 n# _ _ _ _) = n#

{-# INLINE emptyPR_Sum2 #-}
emptyPR_Sum2 pra prb
  = traceFn "emptyPR_Sum2" $
  PSum2 0# emptyPA_Int# emptyPA_Int# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Sum2 #-}
replicatePR_Sum2 pra prb n# p
  = traceFn "replicatePR_Sum2" $
      PSum2 n# (replicatePA_Int# n# (case p of Alt2_1 _ -> 0#
                                               Alt2_2 _ -> 1#))
             (upToPA_Int# n#)
             (case p of Alt2_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt2_2 y -> replicatePR prb n# y
                        _        -> emptyPR prb)

{-# INLINE replicatelPR_Sum2 #-}
replicatelPR_Sum2 pra prb n# mults (PSum2 m# sel# is# as bs)
  = traceFn "replicatelPR_Sum2" $
    PSum2 n# sel' is' as' bs'  
  where
    as'       = replicatelPR pra an1# alt1mults as
    bs'       = replicatelPR prb (n# -# an1#) alt2mults bs
    sel'      = replicatelPA_Int# n# sel# mults
    alt1mults = packPA_Int# mults n# (selectPA_Int# sel# 0#)
    alt2mults = packPA_Int# mults n# (selectPA_Int# sel# 1#)
    an1#      = sumPA_Int# alt1mults
    is'       = selectorToIndices2PA# sel'

{-# INLINE repeatPR_Sum2 #-}
repeatPR_Sum2 pra prb n# (PSum2 m# sel# is# as bs)
  = traceFn "repeatPR_Sum2" $
    PSum2 (m# *# n#) sel' is' as' bs'
  where
    as'  = repeatPR pra n# as
    bs'  = repeatPR prb n# bs
    sel' = repeatPA_Int# n# sel#
    is'  = selectorToIndices2PA# sel'

{-# INLINE indexPR_Sum2 #-}
indexPR_Sum2 pra prb (PSum2 n# sel# is# as bs) i#
  = traceFn "indexPR_Sum2" $
  case indexPA_Int# sel# i# of
      0# -> Alt2_1 (indexPR pra as (indexPA_Int# is# i#))
      _  -> Alt2_2 (indexPR prb bs (indexPA_Int# is# i#))

bpermutePR_Sum2 :: PR a -> PR b -> Int# -> PArray (Sum2 a b) -> PArray_Int# -> PArray (Sum2 a b)
bpermutePR_Sum2 pra prb _ _ is = traceFn "bpermutePR_Sum2" $
                error "bpermutePR_Sum2 nyi"

appPR_Sum2 pra prb (PSum2 n1# sel1# _ as1 bs1) (PSum2 n2# sel2# _ as2 bs2) = traceFn "appPR_Sum2" $           
  PSum2 (n1# +# n2#) (appPA_Int# sel1# sel2#) (error "ind in appPR_Sum2 nyi") (appPR pra as1 as2) (appPR prb bs1 bs2)


applPR_Sum2 pra prb _ _  = error "applPR_Sum2 nyi"
 
packPR_Sum2 :: PR a -> PR b -> PArray (Sum2 a b) -> Int# -> PArray_Bool# -> PArray (Sum2 a b)
packPR_Sum2 pra prb  (PSum2 n# sel# _ as bs) m# flags
  = traceFn "packPR_Sum2" $
    PSum2 m# sel' is as' bs'
  where
    sel'   = packPA_Int# sel# m# flags

    aFlags = packPA_Bool# flags (lengthPR pra as) (selectPA_Int# sel# 0#)
    bFlags = packPA_Bool# flags (lengthPR prb bs) (selectPA_Int# sel# 1#)
    k#     = truesPA_Bool# bFlags

    as'    = packPR pra as (m# -# k#) aFlags
    bs'    = packPR prb bs k# bFlags
    is     = error "packPR_Sum2 index not impl"

combine2PR_Sum2:: PR a -> PR b -> Int# -> PArray_Int# -> PArray_Int#
                              -> PArray (Sum2 a b) -> PArray (Sum2 a b) -> PArray (Sum2 a b)
combine2PR_Sum2 pra prb n# sel# is# (PSum2 m1# sel1# _ as1 bs1) (PSum2 m2# sel2# _ as2 bs2) = traceFn "combine2PR_Sum2" $                
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
                        as# = lengthPR pra as1 +#   lengthPR pra as2
                        bs# = lengthPR prb bs1 +#   lengthPR prb bs2
                        asel = packPA_Int# sel# as# s1#
                        bsel = packPA_Int# sel# bs# s2#
                        as' = trace ("cb1: " ++ show asel) $ combine2PR pra as# asel  is# as1 as2 
                        bs' = trace ("cb2: " ++ show bsel) $ combine2PR prb bs# bsel is# bs1 bs2
     where
       sel' = combine2PA_Int# n# sel# is#  sel1# sel2#
       sel'Bool  = selectPA_Int# sel' 0#
       nsel'Bool = selectPA_Int# sel' 1#


dPR_Sum3 :: PR a -> PR b -> PR c -> PR (Sum3 a b c)
{-# INLINE dPR_Sum3 #-}
dPR_Sum3 pra prb prc
  = PR {
     lengthPR    = lengthPR_Sum3
   , emptyPR     = emptyPR_Sum3 pra prb prc
   , replicatePR = replicatePR_Sum3 pra prb prc
   , indexPR     = indexPR_Sum3 pra prb prc
   }

{-# INLINE lengthPR_Sum3 #-}
lengthPR_Sum3 (PSum3 n# _ _ _ _ _) = n#

{-# INLINE emptyPR_Sum3 #-}
emptyPR_Sum3 pra prb prc        
  = traceFn "emptyPR_Sum3\n" $
  PSum3 0# emptyPA_Int# emptyPA_Int# (emptyPR pra)
                                       (emptyPR prb)
                                       (emptyPR prc)

{-# INLINE replicatePR_Sum3 #-}
replicatePR_Sum3 pra prb prc n# p
  = traceFn "replicatePR_Sum3\n" $
  PSum3 n# (replicatePA_Int# n# (case p of Alt3_1 _ -> 0#
                                           Alt3_2 _ -> 1#
                                           Alt3_3 _ -> 2#))
             (upToPA_Int# n#)
             (case p of Alt3_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt3_2 x -> replicatePR prb n# x
                        _        -> emptyPR prb)
             (case p of Alt3_3 x -> replicatePR prc n# x
                        _        -> emptyPR prc)

{-# INLINE indexPR_Sum3 #-}
indexPR_Sum3 pra prb prc (PSum3 n# sel# is# as bs cs) i#
  = traceFn "indexPR_Sum3\n" $
  case indexPA_Int# sel# i# of
      0# -> Alt3_1 (indexPR pra as (indexPA_Int# is# i#))
      1# -> Alt3_2 (indexPR prb bs (indexPA_Int# is# i#))
      _  -> Alt3_3 (indexPR prc cs (indexPA_Int# is# i#))

data instance PArray (PArray a)
  = PNested Int# PArray_Int# PArray_Int# (PArray a)

dPR_PArray :: PR a -> PR (PArray a)
{-# INLINE dPR_PArray #-}
dPR_PArray pr = PR {
                  lengthPR     = lengthPR_PArray
                , emptyPR      = emptyPR_PArray pr
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

{-# INLINE lengthPR_PArray #-}
lengthPR_PArray (PNested n# _ _ _) = n#

{-# INLINE nested_lengthPA #-}
nested_lengthPA xss = traceFn "nested_lengthPA\n" $
                I# (lengthPR_PArray xss)

{-# INLINE emptyPR_PArray #-}
emptyPR_PArray pr = traceFn "emptyPR_PArray\n" $
               PNested 0# emptyPA_Int# emptyPA_Int# (emptyPR pr)

{-# INLINE replicatePR_PArray #-}
replicatePR_PArray pr n# xs
  = traceFn "replicatePR_PArray\n" $
  PNested n# lens
               (unsafe_scanPA_Int# (+) 0 lens)
               (repeatPR pr n# xs)
  where
    lens = replicatePA_Int# n# (lengthPR pr xs)

{-# INLINE indexPR_PArray #-}
indexPR_PArray pr  (PNested m# lens idxs xs) i#
  = extractPR pr xs (indexPA_Int# idxs i#)
                    (indexPA_Int# lens i#)

{-# INLINE extractPR_PArray #-}
extractPR_PArray pr (PNested m# lens idxs xs) i# n#
  = PNested n# lens' idxs' (extractPR pr xs (indexPA_Int# idxs i#)
                                            (sumPA_Int# lens'))
  where
    lens' = extractPA_Int# lens i# n#
    idxs' = unsafe_scanPA_Int# (+) 0 lens'

{-# INLINE bpermutePR_PArray #-}
-- FIXME: this doesn't look right
bpermutePR_PArray pr m# (PNested n# xslens xsInds xs) is = traceFn "bpermutePR_PArray\n" $
                  PNested n# xslens' xsInds' xs'
  where
    xslens' = bpermutePA_Int# xslens is
    xsInds' = unsafe_scanPA_Int# (+) 0 xslens'
    is1     = bpermutePA_Int# xsInds is  
    is2     = unsafe_zipWithPA_Int# (\x -> \y -> x + y - 1) xslens' is1
    ps      = enumFromToEachPA_Int# (lengthPR pr xs) is1 is2
    xs'     = bpermutePR pr (lengthPA_Int# ps) xs ps

    _dummy :: Int#
    _dummy = m#

{-# INLINE appPR_PArray #-}
appPR_PArray pr (PNested n# xslens xsInds xs) (PNested m# yslens ysInds ys) = traceFn "appPR_PArray\n" $             
   PNested (n# +# m#) (appPA_Int# xslens yslens) (appPA_Int# xsInds ysInds)  (appPR pr xs ys)

{-# INLINE applPR_PArray #-}
-- applPR_PArray:: PR a -> USegd -> PArray a -> USegd -> PArray a -> PArray a
applPR_PArray pr is1  xn@(PNested n# xslens xsInds xs) is2 yn@(PNested m# yslens ysInds ys) = traceFn "applPR_PArray\n" $
   traceArgs ("applPR_PArray:\n" ++
     show is1 ++ "\n" ++          
     show xslens ++ "\n" ++          
     show is2 ++ "\n" ++          
     show yslens ++ "\n" ++          
     show lens) $
   PNested (n# +# m#) lens ids xys
   where 
     lens   = appPA_Int#  xslens yslens 
     xsSegd = sumPAs_Int# is1 xslens
     ysSegd = sumPAs_Int# is2 yslens
     ids    = unsafe_scanPA_Int# (+) 0 lens
     xlen# = lengthPR pr xs
     ylen# = lengthPR pr ys
     len#  = xlen# +# ylen#
     (PNested _ _ _ xys)   = combine2PR_PArray pr len# isel (error "tmp ind nyi") 
       (PNested n# xsSegd (error "bla1") xs)  (PNested n# ysSegd (error "bla1") ys)
     isel   = unsafe_mapPA_Int# (fromBool . even)
            $ enumFromToPA_Int# 1# (2# *# lengthPA_Int# xslens)



{-# INLINE repeatPR_PArray #-}
repeatPR_PArray pr n# (PNested m# lens _ xs)
  = traceFn "repeatPR_PArray\n" $
  PNested (m# *# n#) lens'
                       (unsafe_scanPA_Int# (+) 0 lens')
                       (repeatPR pr n# xs)
  where
    lens' = repeatPA_Int# n# lens

{-# INLINE replicatelPR_PArray #-}
replicatelPR_PArray pr n# ns (PNested _ lens idxs xs)
  = traceFn "replicatelPR_PArray\n" $
  PNested n# new_lens new_idxs (bpermutePR pr len xs indices)
  where
    new_lens = replicatelPA_Int# n# ns lens
    new_idxs = unsafe_scanPA_Int# (+) 0 new_lens
    starts = replicatelPA_Int# n# ns idxs
    ends   = replicatelPA_Int# n# ns
           $ unsafe_zipWithPA_Int# (\i l -> i+l-1) idxs lens

    len     = sumPA_Int# (unsafe_zipWithPA_Int# (*) ns lens)
    indices = enumFromToEachPA_Int# len starts ends

{-# INLINE packPR_PArray #-}
packPR_PArray pr (PNested _ lens _ xs) n# bs
  = traceFn "packPR_PArray\n" $
  PNested n# lens' idxs'
     (packPR pr xs (sumPA_Int# lens')
                   (replicatelPA_Bool# (lengthPR pr xs) lens bs))
  where
    lens' = packPA_Int# lens n# bs
    idxs' = unsafe_scanPA_Int# (+) 0 lens'

{-# INLINE combine2PR_PArray #-}
combine2PR_PArray pr n# sel is (PNested m1# lens1 idxs1 xs)
                               (PNested m2# lens2 idxs2 ys)
  = traceFn ("combine2PR_PArray") $
    traceArgs ("combine2PR_PArray args" ++ show (I# n#) ++ " "  
                                        ++ show (I# m1#) ++ "\n "  
                                        ++ show (I# m2#) ++ "\n "  
                                        ++ show (lens1) ++ "\n"  
                                        ++ show (lens2) ++ "\n"  
                                        ++ show (sel') ++ "\n"  
                                        ++ show (lens) ++ "\n"  
                                        ++ show sel ++ "\n") $ 
      PNested n# lens idxs xys
  where 
    xys  = combine2PR pr len# sel' is' xs ys
    lens = combine2PA_Int# (m1# +# m2#) sel is lens1 lens2
    idxs = unsafe_scanPA_Int# (+) 0 lens

    xlen# = lengthPR pr xs
    ylen# = lengthPR pr ys
    len#  = xlen# +# ylen#

    sel' = replicatelPA_Int# len# lens sel
    is'  = selectorToIndices2PA# sel'

concatPA# :: PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA# #-}
concatPA# (PNested _ _ _ xs) = traceFn "concatPA\n" $
          xs

