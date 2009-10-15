{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Instances (
  PData(..),
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )
import Data.Array.Parallel.Lifted.Selector

import qualified Data.Array.Parallel.Unlifted as U

import GHC.Exts    ( Int#, Int(..), (+#), (*#),
                     Double#, Double(..) )
import GHC.Word    ( Word8(..) )

newtype instance PData Int = PInt (U.Array Int)

type instance PRepr Int = Int

instance PA Int where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

instance PR Int where
  {-# INLINE emptyPR #-}
  emptyPR = PInt U.empty

  {-# INLINE replicatePR #-}
  replicatePR n# i = PInt (U.replicate (I# n#) i)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PInt xs) = PInt (U.replicate_s segd xs)

  {-# INLINE repeatPR #-}
  repeatPR n# len# (PInt xs) = PInt (U.repeat (I# n#) (I# len#) xs)

  {-# INLINE repeatcPR #-}
  repeatcPR n# ns segd (PInt xs) = PInt (U.repeat_c (I# n#) ns segd xs)

  {-# INLINE indexPR #-}
  indexPR (PInt xs) i# = xs U.!: I# i#

  {-# INLINE extractPR #-}
  extractPR (PInt xs) i# n# = PInt (U.extract xs (I# i#) (I# n#))

  {-# INLINE bpermutePR #-}
  bpermutePR (PInt xs) _ is = PInt (U.bpermute xs is)

  {-# INLINE appPR #-}
  appPR (PInt xs) (PInt ys) = PInt (xs U.+:+ ys)

  {-# INLINE applPR #-}
  applPR xsegd (PInt xs) ysegd (PInt ys)
    = PInt (U.append_s xsegd xs ysegd ys)

  {-# INLINE packPR #-}
  packPR (PInt ns) n# bs = PInt (U.pack ns bs)

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PInt xs) (PInt ys)
    = PInt (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PInt (U.fromList xs)

  {-# INLINE nfPR #-}
  nfPR (PInt xs) = xs `seq` ()

{-
upToPA_Int :: Int -> PArray Int
{-# INLINE_PA upToPA_Int #-}
upToPA_Int (I# n#) = PInt n# (upToPA_Int# n#)
-}

newtype instance PData Word8 = PWord8 (U.Array Word8)

type instance PRepr Word8 = Word8

instance PA Word8 where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

instance PR Word8 where
  {-# INLINE emptyPR #-}
  emptyPR = PWord8 U.empty

  {-# INLINE replicatePR #-}
  replicatePR n# i = PWord8 (U.replicate (I# n#) i)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PWord8 xs) = PWord8 (U.replicate_s segd xs)

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

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PWord8 xs) (PWord8 ys)
    = PWord8 (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PWord8 (U.fromList xs)

  {-# INLINE nfPR #-}
  nfPR (PWord8 xs) = xs `seq` ()


newtype instance PData Double = PDouble (U.Array Double)

type instance PRepr Double = Double

instance PA Double where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

instance PR Double where
  {-# INLINE emptyPR #-}
  emptyPR = PDouble U.empty

  {-# INLINE replicatePR #-}
  replicatePR n# i = PDouble (U.replicate (I# n#) i)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (PDouble xs) = PDouble (U.replicate_s segd xs)

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

  {-# INLINE combine2PR #-}
  combine2PR n# sel (PDouble xs) (PDouble ys)
    = PDouble (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

  {-# INLINE fromListPR #-}
  fromListPR n# xs = PDouble (U.fromList xs)

  {-# INLINE nfPR #-}
  nfPR (PDouble xs) = xs `seq` ()


data instance PData Bool
  = PBool Sel2

type instance PRepr Bool = Sum2 Void Void

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False = Alt2_1 void
  toPRepr True  = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _) = False
  fromPRepr (Alt2_2 _) = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _) = PBool sel

{-
toPrimArrPA_Bool :: PArray Bool -> U.Array Bool
{-# INLINE toPrimArrPA_Bool #-}
toPrimArrPA_Bool (PBool sel _ _ _ _ _) = U.pick sel 1

truesPA# :: PArray Bool -> Int#
{-# INLINE_PA truesPA# #-}
truesPA# (PBool _ _ _ fs ts) = lengthPA# dPA_Void ts
-}

{-
data instance PArray Bool = PBool Int# PArray_Int# PArray_Int#

type instance PRepr Bool = Enumeration

dPA_Bool :: PA Bool
{-# INLINE_PA dPA_Bool #-}
dPA_Bool = PA {
             toPRepr      = toPRepr_Bool
           , fromPRepr    = fromPRepr_Bool
           , toArrPRepr   = toArrPRepr_Bool
           , fromArrPRepr = fromArrPRepr_Bool
           , dictPRepr    = dPR_Enumeration
           }

{-# INLINE toPRepr_Bool #-}
toPRepr_Bool False = Enumeration 0#
toPRepr_Bool True  = Enumeration 1#

{-# INLINE fromPRepr_Bool #-}
fromPRepr_Bool (Enumeration 0#) = False
fromPRepr_Bool _                = True

{-# INLINE toArrPRepr_Bool #-}
toArrPRepr_Bool (PBool n# sel# is#) = PEnum n# sel# is#

{-# INLINE fromArrPRepr_Bool #-}
fromArrPRepr_Bool (PEnum n# sel# is#) = PBool n# sel# is#
-}

-- Tuples
--
-- We can use one of the following two representations
--
-- data instance PArray (a1,...,an) = PTup<n> !Int (STup<n> (PArray a1)
--                                                          ...
--                                                          (PArray an))
--
-- where STup<n> are strict n-ary tuples or
--
-- data instance PArray (a1,...,an) = PTup<n> !Int (PArray a1) ... (PArray an)
--
-- Consider the following two terms:
--
--   xs = replicateP n (_|_, _|_)
--   ys = replicateP n (_|_ :: (t,u))
--
-- These have to be represented differently; in particular, we have
--
--   xs !: 0 = (_|_,_|_)
--   ys !: 0 = _|_
--
-- but
--
--   lengthP xs = lengthP ys = n
--
-- With he first representation, we have
--
--   xs = PTup2 n (STup2 (replicateP n _|_) (replicateP n _|_))
--   ys = PTup2 n _|_
--
-- With
-- 
--   PTup2 n (STup2 xs ys) !: i = (xs !: i, ys !: i)
--   lengthP (PTup2 n _)        = n
--
-- this gives use the desired result. With the second representation we might
-- use:
--
--   replicateP n p = PArray n (p `seq` replicateP n x)
--                             (p `seq` replicateP n y)
--     where
--       (x,y) = p
--
-- which gives us
--
--   xs = PTup2 n (replicateP n _|_) (replicateP n _|_)
--   ys = PTup2 n _|_ _|_
--
-- We'd then have to use
--
--   PTup2 n xs ys !: i  = xs `seq` ys `seq` (xs !: i, ys !: i)
--   lengthP (PTup2 n _) = n
--
-- Not sure which is better (the first seems slightly cleaner) but we'll go
-- with the second repr for now as it makes closure environments slightly
-- simpler to construct and to take apart.

{-
data STup2 a b = STup2 !a !b
data STup3 a b c = STup3 !a !b !c
data STup4 a b c d = STup4 !a !b !c !d
data STup5 a b c d e = STup5 !a !b !c !d !e
-}

type instance PRepr () = ()

instance PA () where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

type instance PRepr (a,b) = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a,b) where
  toPRepr (a,b) = (Wrap a, Wrap b)
  fromPRepr (Wrap a, Wrap b) = (a,b)
  toArrPRepr (P_2 as bs) = P_2 (PWrap as) (PWrap bs)
  fromArrPRepr (P_2 (PWrap as) (PWrap bs)) = P_2 as bs

type instance PRepr (a,b,c) = (Wrap a, Wrap b, Wrap c)

instance (PA a, PA b, PA c) => PA (a,b,c) where
  toPRepr (a,b,c) = (Wrap a, Wrap b, Wrap c)
  fromPRepr (Wrap a, Wrap b, Wrap c) = (a,b,c)
  toArrPRepr (P_3 as bs cs) = P_3 (PWrap as) (PWrap bs) (PWrap cs)
  fromArrPRepr (P_3 (PWrap as) (PWrap bs) (PWrap cs)) = P_3 as bs cs

type instance PRepr (a,b,c,d) = (Wrap a, Wrap b, Wrap c, Wrap d)

instance (PA a, PA b, PA c, PA d) => PA (a,b,c,d) where
  toPRepr (a,b,c,d) = (Wrap a, Wrap b, Wrap c, Wrap d)
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d) = (a,b,c,d)
  toArrPRepr (P_4 as bs cs ds)
    = P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds)
  fromArrPRepr (P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds))
    = P_4 as bs cs ds

type instance PRepr (a,b,c,d,e) = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

instance (PA a, PA b, PA c, PA d, PA e) => PA (a,b,c,d,e) where
  toPRepr (a,b,c,d,e) = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e) = (a,b,c,d,e)
  toArrPRepr (P_5 as bs cs ds es)
    = P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) 
  fromArrPRepr (P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es))
    = P_5 as bs cs ds es

type instance PRepr (PArray a) = PArray (PRepr a)

instance PA a => PA (PArray a) where
  {-# INLINE toPRepr #-}
  toPRepr (PArray n# xs) = PArray n# (toArrPRepr xs)

  {-# INLINE fromPRepr #-}
  fromPRepr (PArray n# xs) = PArray n# (fromArrPRepr xs)

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PNested segd xs) = PNested segd (toArrPRepr xs)

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PNested segd xs) = PNested segd (fromArrPRepr xs)

