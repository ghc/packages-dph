{-# LANGUAGE EmptyDataDecls #-}
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

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Selector
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base ((:*:)(..), fromBool)

import qualified Data.List as L
import GHC.Exts  (Int#, Int(..), (+#), (-#), (*#))
import GHC.Word  ( Word8 )
import Debug.Trace


traceFn :: String -> String -> a -> a
-- traceFn fn ty x = trace (fn ++ "<" ++ ty ++ ">") x
traceFn _ _ x = x

traceArg :: Show a => String -> a -> b -> b
-- traceArg name arg x = trace ("    " ++ name ++ " = " ++ show arg) x
traceArg _ _ x = x

---------------------
-- Primitive types --

newtype instance PData Int = PInt (U.Array Int)

instance PR Int where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR" "Int"
          $ PInt U.empty

  {-# INLINE replicatePR #-}
  replicatePR n# i = traceFn "replicatePR" "Int"
                   $ traceArg "n#" (I# n#)
                   $ traceArg "i" i
                   $ PInt (U.replicate (I# n#) i)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PInt xs) = traceFn  "replicatelPR" "Int"
                              $ PInt (U.replicate_s segd xs)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (PInt xs) = traceFn "repeatPR" "Int"
                             $ traceArg "n#" (I# n#)
                             $ traceArg "len#" (I# len#)
                             $ PInt (U.repeat (I# n#) (I# len#) xs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (PInt xs) = traceFn "repeatcPR" "Int"
                                 $ traceArg "n#" (I# n#)
                                 $ PInt (U.repeat_c (I# n#) ns segd xs)

  {-# INLINE indexPR #-}
  indexPR (PInt xs) i# = traceFn "indexPR" "Int"
                       $ traceArg "i#" (I# i#)
                       $ xs U.!: I# i#

  {-# INLINE extractPR #-}
  extractPR (PInt xs) i# n# = traceFn "extractPR" "Int"
                            $ traceArg "i#" (I# i#)
                            $ traceArg "n#" (I# n#)
                            $ PInt (U.extract xs (I# i#) (I# n#))

  {-# INLINE bpermutePR #-}
  bpermutePR (PInt xs) _ is = traceFn "bpermutePR" "Int"
                            $ PInt (U.bpermute xs is)

  {-# INLINE appPR #-}
  appPR (PInt xs) (PInt ys) = traceFn "appPR" "Int"
                            $ PInt (xs U.+:+ ys)

  {-# INLINE applPR #-}
  applPR xsegd (PInt xs) ysegd (PInt ys)
    = traceFn "applPR" "Int"
    $ PInt (U.append_s xsegd xs ysegd ys)

  {-# INLINE packPR #-}
  packPR (PInt ns) n# bs = traceFn "packPR" "Int"
                         $ traceArg "n#" (I# n#)
                         $ PInt (U.pack ns bs)

  {-# INLINE packByTagPR #-}
  packByTagPR (PInt ns) n# tags t# = traceFn "packByTagPR" "Int"
                                   $ traceArg "n#" (I# n#)
                                   $ traceArg "t#" (I# t#)
                                   $ PInt (U.packByTag ns tags (I# t#))

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PInt xs) (PInt ys)
    = PInt (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PInt (U.fromList xs)

  {-# INLINE nfPR #-}
  nfPR (PInt xs) = xs `seq` ()


newtype instance PData Word8 = PWord8 (U.Array Word8)

instance PR Word8 where
  {-# INLINE emptyPR #-}
  emptyPR = PWord8 U.empty

  {-# INLINE replicatePR #-}
  replicatePR n# i = traceFn "replicatePR" "Word8"
                   $ PWord8 (U.replicate (I# n#) i)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PWord8 xs) = traceFn "replicatelPR" "Word8"
                                $ PWord8 (U.replicate_s segd xs)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (PWord8 xs) = PWord8 (U.repeat (I# n#) (I# len#) xs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (PWord8 xs) = PWord8 (U.repeat_c (I# n#) ns segd xs)

  {-# INLINE indexPR #-}
  indexPR (PWord8 xs) i# = xs U.!: I# i#

  {-# INLINE extractPR #-}
  extractPR (PWord8 xs) i# n# = PWord8 (U.extract xs (I# i#) (I# n#))

  {-# INLINE bpermutePR #-}
  bpermutePR (PWord8 xs) _ is = PWord8 (U.bpermute xs is)

  {-# INLINE appPR #-}
  appPR (PWord8 xs) (PWord8 ys) = PWord8 (xs U.+:+ ys)

  {-# INLINE applPR #-}
  applPR xsegd (PWord8 xs) ysegd (PWord8 ys)
    = PWord8 (U.append_s xsegd xs ysegd ys)

  {-# INLINE packPR #-}
  packPR (PWord8 ns) n# bs = PWord8 (U.pack ns bs)

  {-# INLINE packByTagPR #-}
  packByTagPR (PWord8 ns) n# tags t# = PWord8 (U.packByTag ns tags (I# t#))

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PWord8 xs) (PWord8 ys)
    = PWord8 (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PWord8 (U.fromList xs)

  {-# INLINE nfPR #-}
  nfPR (PWord8 xs) = xs `seq` ()

newtype instance PData Double = PDouble (U.Array Double)

instance PR Double where
  {-# INLINE emptyPR #-}
  emptyPR = PDouble U.empty

  {-# INLINE replicatePR #-}
  replicatePR n# i = PDouble (U.replicate (I# n#) i)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PDouble xs)
    = PDouble
    $ traceFn "replicatelPR" "Double"
    $ traceArg "lengthSegd segd" (U.lengthSegd segd)
    $ traceArg "elementsSegd segd" (U.elementsSegd segd)
    $ traceArg "length xs" (U.length xs)
    $ (U.replicate_s segd xs)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (PDouble xs) = PDouble (U.repeat (I# n#) (I# len#) xs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (PDouble xs) = PDouble (U.repeat_c (I# n#) ns segd xs)

  {-# INLINE indexPR #-}
  indexPR (PDouble xs) i# = xs U.!: I# i#

  {-# INLINE extractPR #-}
  extractPR (PDouble xs) i# n# = PDouble (U.extract xs (I# i#) (I# n#))

  {-# INLINE bpermutePR #-}
  bpermutePR (PDouble xs) _ is = PDouble (U.bpermute xs is)

  {-# INLINE appPR #-}
  appPR (PDouble xs) (PDouble ys) = PDouble (xs U.+:+ ys)

  {-# INLINE applPR #-}
  applPR xsegd (PDouble xs) ysegd (PDouble ys)
    = PDouble (U.append_s xsegd xs ysegd ys)

  {-# INLINE packPR #-}
  packPR (PDouble ns) n# bs = PDouble (U.pack ns bs)

  {-# INLINE packByTagPR #-}
  packByTagPR (PDouble ns) n# tags t# = PDouble (U.packByTag ns tags (I# t#))

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PDouble xs) (PDouble ys)
    = PDouble (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PDouble (U.fromList xs)

  {-# INLINE nfPR #-}
  nfPR (PDouble xs) = xs `seq` ()

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

instance PR Void where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR" "Void" $ pvoid

  {-# INLINE replicatePR #-}
  replicatePR _ _ = traceFn "replicatePR" "Void" $ pvoid

  {-# INLINE replicatelPR #-}
  replicatelPR segd _ = traceFn "replicatelPR" "Void" $ pvoid

  {-# INLINE repeatPR #-}
  repeatPR _ _ _ = traceFn "repeatPR" "Void" $ pvoid

  {-# INLINE repeatcPR #-}
  repeatcPR _ _ _ _ = traceFn "repeatcPR" "Void" $ pvoid

  {-# INLINE indexPR #-}
  indexPR _ _ = traceFn "indexPR" "Void" $ void

  {-# INLINE extractPR #-}
  extractPR  _ _ _ = traceFn "extractPR" "Void" $ pvoid

  {-# INLINE bpermutePR #-}
  bpermutePR _ _ _ = traceFn "bpermutePR" "Void" $ pvoid

  {-# INLINE appPR #-}
  appPR  _ _ = traceFn "appPR" "Void" $ pvoid

  {-# INLINE applPR #-}
  applPR _ _ _ _ = traceFn "applPR" "Void" $ pvoid

  {-# INLINE packPR #-}
  packPR _ _ _ = traceFn "packPR" "Void" $ pvoid

  {-# INLINE packByTagPR #-}
  packByTagPR _ _ _ _ = traceFn "packByTagPR" "Void" $ pvoid

  {-# INLINE combine2PR #-}
  combine2PR _ _ _ _ = traceFn "combine2PR" "Void" $ pvoid

  {-# INLINE fromListPR #-}
  fromListPR _ _ = pvoid

  {-# INLINE nfPR #-}
  nfPR _ = ()

----------
-- Unit --

data instance PData () = PUnit

punit :: PData ()
punit = PUnit

instance PR () where
  {-# INLINE emptyPR #-}
  emptyPR = traceFn "emptyPR" "()" $ PUnit

  {-# INLINE replicatePR #-}
  replicatePR n# () = traceFn "replicatePR" "()"
                    $ traceArg "n#" (I# n#)
                    $ PUnit

  {-# INLINE replicatelPR #-}
  replicatelPR segd u = traceFn "replicatelPR" "()" 
                      $ traceArg "elementsSegd segd" (U.elementsSegd segd)
                      $ u

  {-# INLINE repeatPR #-}
  repeatPR _ _ u = traceFn "repeatPR" "()" $ u

  {-# INLINE repeatcPR #-}
  repeatcPR _ _ _ u = traceFn "repeatcPR" "()" $ u

  {-# INLINE indexPR #-}
  indexPR PUnit _ = traceFn "indexPR" "()" $ ()

  {-# INLINE extractPR #-}
  extractPR u _ _ = traceFn "extractPR" "()" $ u

  {-# INLINE bpermutePR #-}
  bpermutePR u _ _ = traceFn "bpermutePR" "()" $ u

  {-# INLINE appPR #-}
  appPR u v = traceFn "appPR" "()" (u `seq` v)

  {-# INLINE applPR #-}
  applPR _ u _ v = traceFn "applPR" "()" (u `seq` v)

  {-# INLINE packPR #-}
  packPR u _ _ = traceFn "packPR" "()" $ u

  {-# INLINE packByTagPR #-}
  packByTagPR u _ _ _ = u

  {-# INLINE combine2PR #-}
  combine2PR _ _ u v = traceFn "combine2PR" "()" (u `seq` v)

  {-# INLINE fromListPR #-}
  fromListPR _ xs = foldr seq PUnit xs

  {-# INLINE nfPR #-}
  nfPR PUnit = ()

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
  emptyPR = traceFn "emptyPR" "(a,b)" $
            P_2 emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b) = traceFn "replicatePR" "(a,b)"
                       $ traceArg "n#" (I# n#)
                       $
    P_2 (replicatePR n# a)
        (replicatePR n# b)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_2 as bs)
    = traceFn "replicatelPR" "(a,b)" $
      P_2 (replicatelPR segd as)
          (replicatelPR segd bs) 

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_2 as bs)
    = traceFn "repeatPR" "(a,b)" $
      P_2 (repeatPR n# len# as)
          (repeatPR n# len# bs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_2 as bs)
    = traceFn "repeatcPR" "(a,b)" $
      P_2 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)

  {-# INLINE indexPR #-}
  indexPR (P_2 as bs) i# = traceFn "indexPR" "(a,b)" $
                                     (indexPR as i#, indexPR bs i#)

  {-# INLINE extractPR #-}
  extractPR (P_2 as bs) i# n# = traceFn "extractPR" "(a,b)"
                                          P_2 (extractPR as i# n#)
                                              (extractPR bs i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_2 as bs) n# is
    = traceFn "bpermutePR" "(a,b)" $
      P_2 (bpermutePR as n# is) (bpermutePR bs n# is)

  {-# INLINE appPR #-}
  appPR (P_2 as1 bs1) (P_2 as2 bs2)
    = P_2 (appPR as1 as2) (appPR bs1 bs2)

  {-# INLINE applPR #-}
  applPR is (P_2 as1 bs1) js (P_2 as2 bs2)
    = traceFn "applPR" "(a,b)" $
      P_2 (applPR is as1 js as2)
          (applPR is bs1 js bs2)

  {-# INLINE packPR #-}
  packPR (P_2 as bs) n# sel# = traceFn "packPR" "(a,b)" $
         P_2 (packPR as n# sel#)
             (packPR bs n# sel#)

  {-# INLINE packByTagPR #-}
  packByTagPR (P_2 as bs) n# tags t#
    = P_2 (packByTagPR as n# tags t#)
          (packByTagPR bs n# tags t#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_2 as1 bs1) (P_2 as2 bs2)
    = traceFn "combine2PR" "(a,b)" $
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
  emptyPR = traceFn "emptyPR" "(a,b,c)" $
          P_3 emptyPR emptyPR emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b,c)
    = traceFn "replicatePR" "(a,b,c)" $
      P_3 (replicatePR n# a)
          (replicatePR n# b)
          (replicatePR n# c)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_3 as bs cs)
    = traceFn "replicatelPR" "(a,b,c)" $
      P_3 (replicatelPR segd as)
          (replicatelPR segd bs)
          (replicatelPR segd cs)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_3 as bs cs)
    = traceFn "repeatPR" "(a,b,c)" $
      P_3 (repeatPR n# len# as)
          (repeatPR n# len# bs)
          (repeatPR n# len# cs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_3 as bs cs)
    = traceFn "repeatcPR" "(a,b,c)" $
      P_3 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)
          (repeatcPR n# ns segd cs)

  {-# INLINE indexPR #-}
  indexPR (P_3 as bs cs) i#
    = traceFn "indexPR" "(a,b,c)" $
      (indexPR as i#, indexPR bs i#, indexPR cs i#)

  {-# INLINE extractPR #-}
  extractPR (P_3 as bs cs) i# n# = traceFn "extractPR" "(a,b,c)" $
          P_3 (extractPR as i# n#)
              (extractPR bs i# n#)
              (extractPR cs i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_3 as bs cs) n# is
    = traceFn "bpermutePR" "(a,b,c)" $
      P_3 (bpermutePR as n# is)
          (bpermutePR bs n# is)
          (bpermutePR cs n# is)

  {-# INLINE appPR #-}
  appPR (P_3 as1 bs1 cs1) (P_3 as2 bs2 cs2)
    = traceFn "appPR" "(a,b,c)" $
      P_3 (appPR as1 as2)
          (appPR bs1 bs2)
          (appPR cs1 cs2)

  {-# INLINE applPR #-}
  applPR is (P_3 as1 bs1 cs1) js (P_3 as2 bs2 cs2)
    = traceFn "applPR" "(a,b,c)" $
      P_3 (applPR is as1 js as2)
          (applPR is bs1 js bs2)
          (applPR is cs1 js cs2)

  {-# INLINE packPR #-}
  packPR (P_3 as bs cs) n# sel#
    = traceFn "packPR" "(a,b,c)" $
      P_3 (packPR as n# sel#)
          (packPR bs n# sel#)
          (packPR cs n# sel#)

  {-# INLINE packByTagPR #-}
  packByTagPR (P_3 as bs cs) n# tags t#
    = P_3 (packByTagPR as n# tags t#)
          (packByTagPR bs n# tags t#)
          (packByTagPR cs n# tags t#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_3 as1 bs1 cs1)
                                  (P_3 as2 bs2 cs2)
    = traceFn "combine2PR" "(a,b,c)" $
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
  emptyPR = traceFn "emptyPR" "(a,b,c,d)" $
          P_4 emptyPR
              emptyPR
              emptyPR
              emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b,c,d)
    = traceFn "replicatePR" "(a,b,c,d)" $
      P_4 (replicatePR n# a)
          (replicatePR n# b)
          (replicatePR n# c)
          (replicatePR n# d)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_4 as bs cs ds)
    = traceFn "replicatelPR" "(a,b,c,d)" $
      P_4 (replicatelPR segd as)
          (replicatelPR segd bs)
          (replicatelPR segd cs)
          (replicatelPR segd ds)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_4 as bs cs ds)
    = traceFn "repeatPR" "(a,b,c,d)" $
      P_4 (repeatPR n# len# as)
          (repeatPR n# len# bs)
          (repeatPR n# len# cs)
          (repeatPR n# len# ds)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_4 as bs cs ds)
    = traceFn "repeatcPR" "(a,b,c,d)" $
      P_4 (repeatcPR n# ns segd as)
          (repeatcPR n# ns segd bs)
          (repeatcPR n# ns segd cs)
          (repeatcPR n# ns segd ds)

  {-# INLINE indexPR #-}
  indexPR (P_4 as bs cs ds) i#
    = traceFn "indexPR" "(a,b,c,d)" $
      (indexPR as i#,
       indexPR bs i#,
       indexPR cs i#,
       indexPR ds i#)

  {-# INLINE extractPR #-}
  extractPR (P_4 as bs cs ds) i# n#
    = traceFn "extractPR" "(a,b,c,d)" $
        P_4 (extractPR as i# n#)
            (extractPR bs i# n#)
            (extractPR cs i# n#)
            (extractPR ds i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_4 as bs cs ds) n# is
    = traceFn "bpermutePR" "(a,b,c,d)" $
      P_4 (bpermutePR as n# is)
          (bpermutePR bs n# is)
          (bpermutePR cs n# is)
          (bpermutePR ds n# is)

  {-# INLINE appPR #-}
  appPR (P_4 as1 bs1 cs1 ds1) (P_4 as2 bs2 cs2 ds2)
    = traceFn "appPR" "(a,b,c,d)" $
      P_4 (appPR as1 as2)
          (appPR bs1 bs2)
          (appPR cs1 cs2)
          (appPR ds1 ds2)

  {-# INLINE applPR #-}
  applPR is (P_4 as1 bs1 cs1 ds1) js (P_4 as2 bs2 cs2 ds2)
    = traceFn "applPR" "(a,b,c,d)" $
      P_4 (applPR is as1 js as2)
          (applPR is bs1 js bs2)
          (applPR is cs1 js cs2)
          (applPR is ds1 js ds2)

  {-# INLINE packPR #-}
  packPR (P_4 as bs cs ds) n# sel#
    = traceFn "packPR" "(a,b,c,d)" $
      P_4 (packPR as n# sel#)
          (packPR bs n# sel#)
          (packPR cs n# sel#)
          (packPR ds n# sel#)

  {-# INLINE packByTagPR #-}
  packByTagPR (P_4 as bs cs ds) n# tags t#
    = P_4 (packByTagPR as n# tags t#)
          (packByTagPR bs n# tags t#)
          (packByTagPR cs n# tags t#)
          (packByTagPR ds n# tags t#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_4 as1 bs1 cs1 ds1)
                                    (P_4 as2 bs2 cs2 ds2)
    = traceFn "combine2PR" "(a,b,c,d)" $
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
    = traceFn "emptyPR" "(a,b,c,d,e)" $
    P_5 emptyPR
        emptyPR
        emptyPR
        emptyPR
        emptyPR

  {-# INLINE replicatePR #-}
  replicatePR n# (a,b,c,d,e)
    = traceFn "replicatePR" "(a,b,c,d,e)" $
    P_5 (replicatePR n# a)
        (replicatePR n# b)
        (replicatePR n# c)
        (replicatePR n# d)
        (replicatePR n# e)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (P_5 as bs cs ds es)
    = traceFn "replicatelPR" "(a,b,c,d,e)" $
    P_5 (replicatelPR segd as)
        (replicatelPR segd bs)
        (replicatelPR segd cs)
        (replicatelPR segd ds)
        (replicatelPR segd es)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (P_5 as bs cs ds es)
    = traceFn "repeatPR" "(a,b,c,d,e)" $
    P_5 (repeatPR n# len# as)
        (repeatPR n# len# bs)
        (repeatPR n# len# cs)
        (repeatPR n# len# ds)
        (repeatPR n# len# es)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (P_5 as bs cs ds es)
    = traceFn "repeatcPR" "(a,b,c,d,e)" $
    P_5 (repeatcPR n# ns segd as)
        (repeatcPR n# ns segd bs)
        (repeatcPR n# ns segd cs)
        (repeatcPR n# ns segd ds)
        (repeatcPR n# ns segd es)

  {-# INLINE indexPR #-}
  indexPR (P_5 as bs cs ds es) i#
    = traceFn "indexPR" "(a,b,c,d,e)" $
    (indexPR as i#,
     indexPR bs i#,
     indexPR cs i#,
     indexPR ds i#,
     indexPR es i#)

  {-# INLINE extractPR #-}
  extractPR (P_5 as bs cs ds es) i# n#
    = traceFn "extractPR" "(a,b,c,d,e)" $
      P_5 (extractPR as i# n#)
          (extractPR bs i# n#)
          (extractPR cs i# n#)
          (extractPR ds i# n#)
          (extractPR es i# n#)

  {-# INLINE bpermutePR #-}
  bpermutePR (P_5 as bs cs ds es) n# is
    = traceFn "bpermutePR" "(a,b,c,d,e)" $
    P_5 (bpermutePR as n# is)
        (bpermutePR bs n# is)
        (bpermutePR cs n# is)
        (bpermutePR ds n# is)
        (bpermutePR es n# is)

  {-# INLINE appPR #-}
  appPR (P_5 as1 bs1 cs1 ds1 es1)
                              (P_5 as2 bs2 cs2 ds2 es2)
    = traceFn "appPR" "(a,b,c,d,e)" $
    P_5 (appPR as1 as2)
        (appPR bs1 bs2)
        (appPR cs1 cs2)
        (appPR ds1 ds2)
        (appPR es1 es2)

  {-# INLINE applPR #-}
  applPR is (P_5 as1 bs1 cs1 ds1 es1)
                               js (P_5 as2 bs2 cs2 ds2 es2)
    = traceFn "applPR" "(a,b,c,d,e)" $
    P_5 (applPR is as1 js as2)
        (applPR is bs1 js bs2)
        (applPR is cs1 js cs2)
        (applPR is ds1 js ds2)
        (applPR is es1 js es2)

  {-# INLINE packPR #-}
  packPR (P_5 as bs cs ds es) n# sel#
    = traceFn "packPR" "(a,b,c,d,e)" $
    P_5 (packPR as n# sel#)
        (packPR bs n# sel#)
        (packPR cs n# sel#)
        (packPR ds n# sel#)
        (packPR es n# sel#)

  {-# INLINE packByTagPR #-}
  packByTagPR (P_5 as bs cs ds es) n# tags t#
    = P_5 (packByTagPR as n# tags t#)
          (packByTagPR bs n# tags t#)
          (packByTagPR cs n# tags t#)
          (packByTagPR ds n# tags t#)
          (packByTagPR es n# tags t#)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (P_5 as1 bs1 cs1 ds1 es1)
                                          (P_5 as2 bs2 cs2 ds2 es2)
    = traceFn "combine2PR" "(a,b,c,d,e)" $
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

