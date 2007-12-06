module Data.Array.Parallel.Lifted.Repr (
  PArray(..),
  Void, void,
  Wrap(..),
  Enumeration(..),
  Sum2(..), Sum3(..), 

  dPA_Void,
  dPR_Void, dPR_Unit, dPR_Wrap,
  dPR_Enumeration,
  dPR_2, dPR_3, zipPA#,
  dPR_Sum2, dPR_Sum3,

  dPR_PArray, concatPA#
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Unlifted

import GHC.Exts  (Int#, Int(..), (*#))

data Void

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
           , bpermutePR   = bpermutePR_Void
           , packPR       = packPR_Void
           , combine2PR   = combine2PR_Void
           }

{-# INLINE lengthPR_Void #-}
lengthPR_Void (PVoid n#) = n#

{-# INLINE emptyPR_Void #-}
emptyPR_Void = PVoid 0#

{-# INLINE replicatePR_Void #-}
replicatePR_Void n# _ = PVoid n#

{-# INLINE replicatelPR_Void #-}
replicatelPR_Void n# _ _ = PVoid n#

{-# INLINE repeatPR_Void #-}
repeatPR_Void n# (PVoid m#) = PVoid (n# *# m#)

indexPR_Void :: PArray Void -> Int# -> Void
{-# INLINE indexPR_Void #-}
indexPR_Void (PVoid n#) i# = void

{-# INLINE bpermutePR_Void #-}
bpermutePR_Void (PVoid _) is = PVoid (lengthPA_Int# is)

{-# INLINE packPR_Void #-}
packPR_Void (PVoid _) n# _ = PVoid n#

{-# INLINE combine2PR_Void #-}
combine2PR_Void n# _ _ (PVoid _) (PVoid _) = PVoid n#

type instance PRepr Void = Void

dPA_Void :: PA Void
{-# INLINE dPA_Void #-}
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
           , bpermutePR   = bpermutePR_Unit
           , packPR       = packPR_Unit
           , combine2PR   = combine2PR_Unit
           }
         

{-# INLINE lengthPR_Unit #-}
lengthPR_Unit (PUnit n# _) = n#

{-# INLINE emptyPR_Unit #-}
emptyPR_Unit = PUnit 0# ()

{-# INLINE replicatePR_Unit #-}
replicatePR_Unit n# u = PUnit n# u

{-# INLINE replicatelPR_Unit #-}
replicatelPR_Unit n# _ (PUnit _ u) = PUnit n# u

{-# INLINE repeatPR_Unit #-}
repeatPR_Unit n# (PUnit m# u) = PUnit (n# *# m#) u

indexPR_Unit :: PArray () -> Int# -> ()
{-# INLINE indexPR_Unit #-}
indexPR_Unit (PUnit n# u) i# = u

{-# INLINE bpermutePR_Unit #-}
bpermutePR_Unit (PUnit _ u) is = PUnit (lengthPA_Int# is) u

{-# INLINE packPR_Unit #-}
packPR_Unit (PUnit _ u) n# _ = PUnit n# u

{-# INLINE combine2PR_Unit #-}
combine2PR_Unit n# _ _ (PUnit _ u1) (PUnit _ u2)
  = PUnit n# (u1 `seq` u2)

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
            , bpermutePR   = bpermutePR_Wrap pr
            , packPR       = packPR_Wrap pr
            }

{-# INLINE lengthPR_Wrap #-}
lengthPR_Wrap (PWrap n# _) = n#

{-# INLINE emptyPR_Wrap #-}
emptyPR_Wrap pr = PWrap 0# (emptyPR pr)

{-# INLINE replicatePR_Wrap #-}
replicatePR_Wrap pr n# ~(Wrap x) = PWrap n# (replicatePR pr n# x)

{-# INLINE replicatelPR_Wrap #-}
replicatelPR_Wrap pr n# ns (PWrap _ xs) = PWrap n# (replicatelPR pr n# ns xs)

{-# INLINE repeatPR_Wrap #-}
repeatPR_Wrap pr n# (PWrap m# xs) = PWrap (n# *# m#) (repeatPR pr n# xs)

{-# INLINE indexPR_Wrap #-}
indexPR_Wrap pr (PWrap n# xs) i# = Wrap (indexPR pr xs i#)

{-# INLINE bpermutePR_Wrap #-}
bpermutePR_Wrap pr (PWrap n# xs) is = PWrap (lengthPA_Int# is)
                                            (bpermutePR pr xs is)

{-# INLINE packPR_Wrap #-}
packPR_Wrap pr (PWrap _ xs) n# sel# = PWrap n# (packPR pr xs n# sel#)

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
emptyPR_Enumeration = PEnum 0# emptyPA_Int# emptyPA_Int#

{-# INLINE replicatePR_Enumeration #-}
replicatePR_Enumeration n# enum
  = PEnum n# (replicatePA_Int# n# (case enum of { Enumeration i# -> i# }))
             (upToPA_Int# n#)

data instance PArray (a,b)
  = P_2 Int# (PArray a)
             (PArray b)

data instance PArray (a,b,c)
  = P_3 Int# (PArray a)
             (PArray b)
             (PArray c)

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
    , bpermutePR   = bpermutePR_2 pra prb
    , packPR       = packPR_2 pra prb
    , combine2PR   = combine2PR_2 pra prb
    }

{-# INLINE lengthPR_2 #-}
lengthPR_2 (P_2 n# _ _) = n#

{-# INLINE emptyPR_2 #-}
emptyPR_2 pra prb = P_2 0# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_2 #-}
replicatePR_2 pra prb n# ~(a,b)
  = P_2 n# (replicatePR pra n# a)
           (replicatePR prb n# b)

{-# INLINE replicatelPR_2 #-}
replicatelPR_2 pra prb n# ns (P_2 _ as bs)
  = P_2 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs) 

{-# INLINE repeatPR_2 #-}
repeatPR_2 pra prb n# (P_2 m# as bs)
  = P_2 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)

{-# INLINE indexPR_2 #-}
indexPR_2 pra prb (P_2 _ as bs) i# = (indexPR pra as i#, indexPR prb bs i#)

{-# INLINE bpermutePR_2 #-}
bpermutePR_2 pra prb (P_2 _ as bs) is
  = P_2 (lengthPA_Int# is) (bpermutePR pra as is)
                           (bpermutePR prb bs is)

{-# INLINE packPR_2 #-}
packPR_2 pra prb (P_2 _ as bs) n# sel# = P_2 n# (packPR pra as n# sel#)
                                                (packPR prb bs n# sel#)

{-# INLINE combine2PR_2 #-}
combine2PR_2 pra prb n# sel# is# (P_2 _ as1 bs1) (P_2 _ as2 bs2)
  = P_2 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)

zipPA# :: PA a -> PA b -> PArray a -> PArray b -> PArray (a,b)
{-# INLINE zipPA# #-}
zipPA# pa pb xs ys = P_2 (lengthPA# pa xs) xs ys

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
    , bpermutePR   = bpermutePR_3 pra prb prc
    , packPR       = packPR_3 pra prb prc
    , combine2PR   = combine2PR_3 pra prb prc
    }

{-# INLINE lengthPR_3 #-}
lengthPR_3 (P_3 n# _ _ _) = n#

{-# INLINE emptyPR_3 #-}
emptyPR_3 pra prb prc = P_3 0# (emptyPR pra) (emptyPR prb) (emptyPR prc)

{-# INLINE replicatePR_3 #-}
replicatePR_3 pra prb prc n# ~(a,b,c)
  = P_3 n# (replicatePR pra n# a)
           (replicatePR prb n# b)
           (replicatePR prc n# c)

{-# INLINE replicatelPR_3 #-}
replicatelPR_3 pra prb prc n# ns (P_3 _ as bs cs)
  = P_3 n# (replicatelPR pra n# ns as)
           (replicatelPR prb n# ns bs)
           (replicatelPR prc n# ns cs)

{-# INLINE repeatPR_3 #-}
repeatPR_3 pra prb prc n# (P_3 m# as bs cs)
  = P_3 (n# *# m#) (repeatPR pra n# as)
                   (repeatPR prb n# bs)
                   (repeatPR prc n# cs)

{-# INLINE indexPR_3 #-}
indexPR_3 pra prb prc (P_3 n# as bs cs) i#
  = (indexPR pra as i#, indexPR prb bs i#, indexPR prc cs i#)

{-# INLINE bpermutePR_3 #-}
bpermutePR_3 pra prb prc (P_3 _ as bs cs) is
  = P_3 (lengthPA_Int# is) (bpermutePR pra as is)
                           (bpermutePR prb bs is)
                           (bpermutePR prc cs is)
{-# INLINE packPR_3 #-}
packPR_3 pra prb prc (P_3 _ as bs cs) n# sel#
  = P_3 n# (packPR pra as n# sel#)
           (packPR prb bs n# sel#)
           (packPR prc cs n# sel#)

{-# INLINE combine2PR_3 #-}
combine2PR_3 pra prb prc n# sel# is# (P_3 _ as1 bs1 cs1)
                                     (P_3 _ as2 bs2 cs2)
  = P_3 n# (combine2PR pra n# sel# is# as1 as2)
           (combine2PR prb n# sel# is# bs1 bs2)
           (combine2PR prc n# sel# is# cs1 cs2)

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
                     lengthPR    = lengthPR_Sum2
                   , emptyPR     = emptyPR_Sum2 pra prb
                   , replicatePR = replicatePR_Sum2 pra prb
                   , indexPR     = indexPR_Sum2 pra prb
                   }

{-# INLINE lengthPR_Sum2 #-}
lengthPR_Sum2 (PSum2 n# _ _ _ _) = n#

{-# INLINE emptyPR_Sum2 #-}
emptyPR_Sum2 pra prb
  = PSum2 0# emptyPA_Int# emptyPA_Int# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Sum2 #-}
replicatePR_Sum2 pra prb n# p
  = PSum2 n# (replicatePA_Int# n# (case p of Alt2_1 _ -> 0#
                                             Alt2_2 _ -> 1#))
             (upToPA_Int# n#)
             (case p of Alt2_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt2_2 y -> replicatePR prb n# y
                        _        -> emptyPR prb)

{-# INLINE indexPR_Sum2 #-}
indexPR_Sum2 pra prb (PSum2 n# sel# is# as bs) i#
  = case indexPA_Int# sel# i# of
      0# -> Alt2_1 (indexPR pra as (indexPA_Int# is# i#))
      _  -> Alt2_2 (indexPR prb bs (indexPA_Int# is# i#))

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
  = PSum3 0# emptyPA_Int# emptyPA_Int# (emptyPR pra)
                                       (emptyPR prb)
                                       (emptyPR prc)

{-# INLINE replicatePR_Sum3 #-}
replicatePR_Sum3 pra prb prc n# p
  = PSum3 n# (replicatePA_Int# n# (case p of Alt3_1 _ -> 0#
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
  = case indexPA_Int# sel# i# of
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
                }

{-# INLINE lengthPR_PArray #-}
lengthPR_PArray (PNested n# _ _ _) = n#

{-# INLINE emptyPR_PArray #-}
emptyPR_PArray pr = PNested 0# emptyPA_Int# emptyPA_Int# (emptyPR pr)

{-# INLINE replicatePR_PArray #-}
replicatePR_PArray pr n# xs
  = PNested n# lens
               (unsafe_scanPA_Int# (+) 0 lens)
               (repeatPR pr n# xs)
  where
    lens = replicatePA_Int# n# (lengthPR pr xs)

-- FIXME: compute indices more efficiently?
{-# INLINE repeatPR_PArray #-}
repeatPR_PArray pr n# (PNested m# lens _ xs)
  = PNested (m# *# n#) lens'
                       (unsafe_scanPA_Int# (+) 0 lens')
                       (repeatPR pr n# xs)
  where
    lens' = repeatPA_Int# n# lens

{-# INLINE replicatelPR_PArray #-}
replicatelPR_PArray pr n# (PInt# ns) (PNested _ (PInt# lens) (PInt# idxs) xs)
  = PNested n# (PInt# new_lens) (PInt# new_idxs)
               (bpermutePR pr xs (PInt# indices))
  where
    new_lens = concatSU (replicateSU ns lens)
    new_idxs = scanU (+) 0 new_lens
    starts = concatSU (replicateSU ns idxs)
    ends   = concatSU . replicateSU ns
           $ zipWithU (\i l -> i+l-1) idxs lens

    indices = enumFromToEachU (sumU (zipWithU (*) ns lens))
            $ zipU starts ends

concatPA# :: PArray (PArray a) -> PArray a
{-# INLINE concatPA# #-}
concatPA# (PNested _ _ _ xs) = xs

