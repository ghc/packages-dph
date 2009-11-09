{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Repr (
  PData(..),
  Void, void, pvoid, fromVoid,

  punit,
  Wrap(..),

  zipPA#, unzipPA#, zip3PA#,
  Sum2(..), {-Sum3(..),-} {-dPR_Sum3,-}

  segdPA#, concatPA#, segmentPA#, copySegdPA#
) where

import Data.Array.Parallel.Lifted.TH.Repr

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Selector
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base ((:*:)(..), fromBool)
import Data.Array.Parallel.Base.DTrace ( traceFn, traceArg )

import Data.List (unzip4, unzip5)
import GHC.Exts  (Int#, Int(..), (+#), (-#), (*#))
import GHC.Word  ( Word8 )


---------------------
-- Primitive types --

-- Generate
--
-- newtype instance PData Int = PInt (U.Array Int)
--
-- instance Prim Int where
--   fromPrimPData (PInt xs) = xs
--   toPrimPData = PInt
--
-- instance PR Int where
--   <forward to *PRPrim methods>

$(primInstances [''Int, ''Double, ''Word8])

----------
-- Void --

data Void

void :: Void
void = error "Data.Array.Parallel.void"

fromVoid :: a
fromVoid = error "fromPRepr Data.Array.Parallel.void"

data instance PData Void

pvoid :: PData Void
pvoid = error "Data.Array.Parallel.PData Void"

$(voidPRInstance ''Void 'void 'pvoid)

----------
-- Unit --

data instance PData () = PUnit

punit :: PData ()
punit = PUnit

$(unitPRInstance 'PUnit)

----------
-- Wrap --

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

  {-# INLINE packByTagPR #-}
  packByTagPR (PWrap xs) n# tags t# = PWrap (packByTagPD xs n# tags t#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PWrap xs) (PWrap ys)
    = PWrap (combine2PD n# sel xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PWrap (fromListPD n# (map unWrap xs))

  {-# INLINE nfPR #-}
  nfPR (PWrap xs) = nfPD xs

------------
-- Tuples --

$(tupleInstances [2..5])

{-
 - Here is what gets generated
 -

data instance PData (a,b)
  = P_2 (PData a)
        (PData b)

instance (PR a, PR b) => PR (a,b) where
  {-# INLINE emptyPR #-}
  emptyPR = P_2 emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b) = 
      P_2 (replicatePR n# a)
          (replicatePR n# b)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_2 as bs) =
      P_2 (replicatelPR segd as)
          (replicatelPR segd bs) 

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_2 as bs) =
      P_2 (repeatPR n# len# as)
          (repeatPR n# len# bs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_2 as bs) =
      P_2 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)

  {-# INLINE indexPR #-}
  indexPR (P_2 as bs) i# = (indexPR as i#, indexPR bs i#)

  {-# INLINE extractPR #-}
  extractPR (P_2 as bs) i# n# = 
      P_2 (extractPR as i# n#)
          (extractPR bs i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_2 as bs) n# is =
      P_2 (bpermutePR as n# is)
          (bpermutePR bs n# is)

  {-# INLINE appPR #-}
  appPR (P_2 as1 bs1) (P_2 as2 bs2) =
      P_2 (appPR as1 as2) (appPR bs1 bs2)

  {-# INLINE applPR #-}
  applPR is (P_2 as1 bs1) js (P_2 as2 bs2) =
      P_2 (applPR is as1 js as2)
          (applPR is bs1 js bs2)

  {-# INLINE packPR #-}
  packPR (P_2 as bs) n# sel# =
      P_2 (packPR as n# sel#)
          (packPR bs n# sel#)

  {-# INLINE packByTagPR #-}
  packByTagPR (P_2 as bs) n# tags t# =
      P_2 (packByTagPR as n# tags t#)
          (packByTagPR bs n# tags t#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_2 as1 bs1) (P_2 as2 bs2) =
      P_2 (combine2PR n# sel as1 as2)
          (combine2PR n# sel bs1 bs2)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = let (as,bs) = unzip xs in
      P_2 (fromListPR n# as)
          (fromListPR n# bs)

  {-# INLINE nfPR #-}
  nfPR (P_2 as bs) = nfPR as `seq` nfPR bs
-}

zipPA# :: PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA# #-}
zipPA# (PArray n# xs) (PArray _ ys) = PArray n# (P_2 xs ys)

unzipPA# :: PArray (a,b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA# #-}
unzipPA# (PArray n# (P_2 xs ys)) = (PArray n# xs, PArray n# ys)


zip3PA# :: PArray a -> PArray b -> PArray c -> PArray (a,b,c)
{-# INLINE_PA zip3PA# #-}
zip3PA# (PArray n# xs) (PArray _ ys) (PArray _ zs) = PArray n# (P_3 xs ys zs)

----------
-- Sums --

data Sum2 a b = Alt2_1 a | Alt2_2 b
data Sum3 a b c = Alt3_1 a | Alt3_2 b | Alt3_3 c

data instance PData (Sum2 a b)
  = PSum2 Sel2 (PData a) (PData b)

instance (PR a, PR b) => PR (Sum2 a b) where 
  {-# INLINE emptyPR #-}
  emptyPR
    = traceFn "emptyPR" "(Sum2 a b)" $
    PSum2 (mkSel2 U.empty U.empty 0 0) emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (Alt2_1 x)
    = traceFn "replicatePR" "(Sum2 a b)" $
      PSum2 (mkSel2 (U.replicate (I# n#) 0)
                    (U.enumFromStepLen 0 1 (I# n#))
                    (I# n#) 0)
            (replicatePR n# x)
            emptyPR
  replicatePR n# (Alt2_2 x)
    = traceFn "replicatePR" "(Sum2 a b)" $
      PSum2 (mkSel2 (U.replicate (I# n#) 1)
                    (U.enumFromStepLen 0 1 (I# n#))
                    0 (I# n#))
            emptyPR
            (replicatePR n# x)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PSum2 sel as bs)
    = traceFn "replicatelPR" "(Sum2 a b)" $
      PSum2 sel' as' bs'
    where
      tags      = tagsSel2 sel
      tags'     = U.replicate_s segd tags
      sel'      = tagsToSel2 tags'

      lens      = U.lengthsSegd segd

      asegd     = U.lengthsToSegd (U.packByTag lens tags 0)
      bsegd     = U.lengthsToSegd (U.packByTag lens tags 1)

      as'       = replicatelPR asegd as
      bs'       = replicatelPR bsegd bs

  {-# INLINE repeatPR #-}
  repeatPR m# n# (PSum2 sel as bs)
    = traceFn "repeatPR" "(Sum2 a b)" $
      PSum2 sel' as' bs'
    where
      sel' = tagsToSel2
           . U.repeat (I# m#) (I# n#)
           $ tagsSel2 sel

      as'  = repeatPR m# (elementsSel2_0# sel) as
      bs'  = repeatPR n# (elementsSel2_1# sel) bs

  {-# INLINE indexPR #-}
  indexPR (PSum2 sel as bs) i#
    = traceFn "indexPR" "(Sum2 a b)" $
    case indicesSel2 sel U.!: I# i# of
      I# k# -> case tagsSel2 sel U.!: I# i# of
                 0 -> Alt2_1 (indexPR as k#)
                 _ -> Alt2_2 (indexPR bs k#)

  {-# INLINE appPR #-}
  appPR (PSum2 sel1 as1 bs1)
                     (PSum2 sel2 as2 bs2)
    = traceFn "appPR" "(Sum2 a b)" $           
      PSum2 sel (appPR as1 as2)
                (appPR bs1 bs2)
    where
      sel = tagsToSel2
          $ tagsSel2 sel1 U.+:+ tagsSel2 sel2

  {-# INLINE packPR #-}
  packPR (PSum2 sel as bs) m# flags
    = traceFn "packPR" "(Sum2 a b)" $
      PSum2 sel' as' bs'
    where
      tags   = tagsSel2 sel
      tags'  = U.pack tags flags
      sel'   = tagsToSel2 tags'

      aFlags = U.pack flags (U.pick tags 0)
      bFlags = U.pack flags (U.pick tags 1)

      as'    = packPR as (elementsSel2_0# sel') aFlags
      bs'    = packPR bs (elementsSel2_1# sel') bFlags

  {-# INLINE packByTagPR #-}
  packByTagPR (PSum2 sel as bs) n# tags t#
    = PSum2 sel' as' bs'
    where
      my_tags  = tagsSel2 sel
      my_tags' = U.packByTag my_tags tags (I# t#)
      sel'     = tagsToSel2 my_tags'

      atags    = U.packByTag tags my_tags 0
      btags    = U.packByTag tags my_tags 1

      as'      = packByTagPR as (elementsSel2_0# sel') atags t#
      bs'      = packByTagPR bs (elementsSel2_1# sel') btags t#

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PSum2 sel1 as1 bs1)
                                 (PSum2 sel2 as2 bs2)
    = traceFn "combine2PR" "(Sum2 a b)" $
      PSum2 sel' as bs
    where
      tags  = tagsSel2 sel
      tags' = U.combine (U.pick tags 0) (tagsSel2 sel1) (tagsSel2 sel2)
      sel'  = tagsToSel2 tags'

      asel = tagsToSel2 (U.packByTag tags tags' 0)
      bsel = tagsToSel2 (U.packByTag tags tags' 1)

      as   = combine2PR (elementsSel2_0# sel') asel as1 as2
      bs   = combine2PR (elementsSel2_1# sel') bsel bs1 bs2

-------------------
-- Nested arrays --

data instance PData (PArray a) = PNested U.Segd (PData a)

instance PR a => PR (PArray a) where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR" "(PArray a)" $
          PNested (U.mkSegd U.empty U.empty 0) emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (PArray m# xs)
    = traceFn "replicatePR" "(PArray a)" $
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
    = traceFn "bpermutePR" "(PArray a)" $
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
    = traceFn "appPR" "(PArray a)" $             
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
    = traceFn "repeatPR" "(PArray a)" $
    PNested segd' (repeatPR n# (elementsSegd# segd) xs)
    where
      segd' = U.lengthsToSegd (U.repeat (I# n#) (I# len#) (U.lengthsSegd segd))

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PNested xsegd xs)
    = traceFn "replicatelPR" "(PArray a)" $
    PNested xsegd' xs'
    where
      xsegd' = U.lengthsToSegd
             $ U.replicate_s segd (U.lengthsSegd xsegd)

      xs'    = repeatcPR (elementsSegd# xsegd')
                            (U.lengthsSegd segd)
                            xsegd xs

  {-# INLINE packPR #-}
  packPR (PNested segd xs) n# flags
    = traceFn "packPR" "(PArray a)" $
    PNested segd' xs'
    where
      segd' = U.lengthsToSegd
            $ U.pack (U.lengthsSegd segd) flags

      xs'   = packPR xs (elementsSegd# segd') (U.replicate_s segd flags)

  {-# INLINE packByTagPR #-}
  packByTagPR (PNested segd xs) n# tags t#
    = PNested segd' xs'
    where
      segd' = U.lengthsToSegd
            $ U.packByTag (U.lengthsSegd segd) tags (I# t#)

      xs'   = packByTagPR xs (elementsSegd# segd') (U.replicate_s segd tags) t#

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PNested xsegd xs) (PNested ysegd ys)
    = traceFn "combine2PR" "(PArray a)" $
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

