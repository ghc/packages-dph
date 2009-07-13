{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Instances (
  PData(..),

  dPA_Int, dPR_Int, {- upToPA_Int, -}

  dPA_Word8, dPR_Word8,
  dPA_Double, dPR_Double,

  dPA_Bool, {- toPrimArrPA_Bool, truesPA#, -}
  dPA_Unit, dPA_2, dPA_3, dPA_4, dPA_5,
  dPA_PArray
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

dPA_Int :: PA Int
{-# INLINE_PA dPA_Int #-}
dPA_Int = PA {
            toPRepr      = id
          , fromPRepr    = id
          , toArrPRepr   = id
          , fromArrPRepr = id
          , dictPRepr    = dPR_Int
          }

dPR_Int :: PR Int
{-# INLINE dPR_Int #-}
dPR_Int = PR {
            emptyPR      = emptyPR_Int
          , replicatePR  = replicatePR_Int
          , replicatelPR = replicatelPR_Int
          , repeatPR     = repeatPR_Int
          , repeatcPR    = repeatcPR_Int
          , indexPR      = indexPR_Int
          , bpermutePR   = bpermutePR_Int
          , extractPR    = extractPR_Int
          , appPR        = appPR_Int
          , applPR       = applPR_Int
          , packPR       = packPR_Int
          , combine2PR   = combine2PR_Int
          , fromListPR   = fromListPR_Int
          , nfPR         = nfPR_Int
          }

{-# INLINE emptyPR_Int #-}
emptyPR_Int = PInt U.empty

{-# INLINE replicatePR_Int #-}
replicatePR_Int n# i = PInt (U.replicate (I# n#) i)

{-# INLINE replicatelPR_Int #-}
replicatelPR_Int segd (PInt xs) = PInt (U.replicate_s segd xs)

{-# INLINE repeatPR_Int #-}
repeatPR_Int n# len# (PInt xs) = PInt (U.repeat (I# n#) (I# len#) xs)

{-# INLINE repeatcPR_Int #-}
repeatcPR_Int n# ns segd (PInt xs) = PInt (U.repeat_c (I# n#) ns segd xs)

{-# INLINE indexPR_Int #-}
indexPR_Int (PInt xs) i# = xs U.!: I# i#

{-# INLINE extractPR_Int #-}
extractPR_Int (PInt xs) i# n# = PInt (U.extract xs (I# i#) (I# n#))

bpermutePR_Int :: T_bpermutePR Int
{-# INLINE bpermutePR_Int #-}
bpermutePR_Int (PInt xs) _ is = PInt (U.bpermute xs is)

{-# INLINE appPR_Int #-}
appPR_Int (PInt xs) (PInt ys) = PInt (xs U.+:+ ys)

{-# INLINE applPR_Int #-}
applPR_Int xsegd (PInt xs) ysegd (PInt ys)
  = PInt (U.append_s xsegd xs ysegd ys)

packPR_Int :: T_packPR Int
{-# INLINE packPR_Int #-}
packPR_Int (PInt ns) n# bs = PInt (U.pack ns bs)

combine2PR_Int :: T_combine2PR Int
{-# INLINE combine2PR_Int #-}
combine2PR_Int n# sel (PInt xs) (PInt ys)
  = PInt (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

fromListPR_Int :: T_fromListPR Int
{-# INLINE fromListPR_Int #-}
fromListPR_Int n# xs = PInt (U.fromList xs)

{-# INLINE nfPR_Int #-}
nfPR_Int (PInt xs) = xs `seq` ()

{-
upToPA_Int :: Int -> PArray Int
{-# INLINE_PA upToPA_Int #-}
upToPA_Int (I# n#) = PInt n# (upToPA_Int# n#)
-}

newtype instance PData Word8 = PWord8 (U.Array Word8)

type instance PRepr Word8 = Word8

dPA_Word8 :: PA Word8
{-# INLINE_PA dPA_Word8 #-}
dPA_Word8 = PA {
            toPRepr      = id
          , fromPRepr    = id
          , toArrPRepr   = id
          , fromArrPRepr = id
          , dictPRepr    = dPR_Word8
          }

dPR_Word8 :: PR Word8
{-# INLINE dPR_Word8 #-}
dPR_Word8 = PR {
            emptyPR      = emptyPR_Word8
          , replicatePR  = replicatePR_Word8
          , replicatelPR = replicatelPR_Word8
          , repeatPR     = repeatPR_Word8
          , repeatcPR    = repeatcPR_Word8
          , indexPR      = indexPR_Word8
          , extractPR    = extractPR_Word8
          , bpermutePR   = bpermutePR_Word8
          , appPR        = appPR_Word8
          , applPR       = applPR_Word8
          , packPR       = packPR_Word8
          , combine2PR   = combine2PR_Word8
          , fromListPR   = fromListPR_Word8
          , nfPR         = nfPR_Word8
          }

{-# INLINE emptyPR_Word8 #-}
emptyPR_Word8 = PWord8 U.empty

{-# INLINE replicatePR_Word8 #-}
replicatePR_Word8 n# i = PWord8 (U.replicate (I# n#) i)

{-# INLINE replicatelPR_Word8 #-}
replicatelPR_Word8 segd (PWord8 xs) = PWord8 (U.replicate_s segd xs)

{-# INLINE repeatPR_Word8 #-}
repeatPR_Word8 n# len# (PWord8 xs) = PWord8 (U.repeat (I# n#) (I# len#) xs)

{-# INLINE repeatcPR_Word8 #-}
repeatcPR_Word8 n# ns segd (PWord8 xs) = PWord8 (U.repeat_c (I# n#) ns segd xs)

{-# INLINE indexPR_Word8 #-}
indexPR_Word8 (PWord8 xs) i# = xs U.!: I# i#

{-# INLINE extractPR_Word8 #-}
extractPR_Word8 (PWord8 xs) i# n# = PWord8 (U.extract xs (I# i#) (I# n#))

bpermutePR_Word8 :: T_bpermutePR Word8
{-# INLINE bpermutePR_Word8 #-}
bpermutePR_Word8 (PWord8 xs) _ is = PWord8 (U.bpermute xs is)

{-# INLINE appPR_Word8 #-}
appPR_Word8 (PWord8 xs) (PWord8 ys) = PWord8 (xs U.+:+ ys)

{-# INLINE applPR_Word8 #-}
applPR_Word8 xsegd (PWord8 xs) ysegd (PWord8 ys)
  = PWord8 (U.append_s xsegd xs ysegd ys)

packPR_Word8 :: T_packPR Word8
{-# INLINE packPR_Word8 #-}
packPR_Word8 (PWord8 ns) n# bs = PWord8 (U.pack ns bs)

combine2PR_Word8 :: T_combine2PR Word8
{-# INLINE combine2PR_Word8 #-}
combine2PR_Word8 n# sel (PWord8 xs) (PWord8 ys)
  = PWord8 (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

fromListPR_Word8 :: T_fromListPR Word8
{-# INLINE fromListPR_Word8 #-}
fromListPR_Word8 n# xs = PWord8 (U.fromList xs)

{-# INLINE nfPR_Word8 #-}
nfPR_Word8 (PWord8 xs) = xs `seq` ()


newtype instance PData Double = PDouble (U.Array Double)

type instance PRepr Double = Double

dPA_Double :: PA Double
{-# INLINE_PA dPA_Double #-}
dPA_Double = PA {
            toPRepr      = id
          , fromPRepr    = id
          , toArrPRepr   = id
          , fromArrPRepr = id
          , dictPRepr    = dPR_Double
          }

dPR_Double :: PR Double
{-# INLINE dPR_Double #-}
dPR_Double = PR {
            emptyPR      = emptyPR_Double
          , replicatePR  = replicatePR_Double
          , replicatelPR = replicatelPR_Double
          , repeatPR     = repeatPR_Double
          , repeatcPR    = repeatcPR_Double
          , indexPR      = indexPR_Double
          , extractPR    = extractPR_Double
          , bpermutePR   = bpermutePR_Double
          , appPR        = appPR_Double
          , applPR       = applPR_Double
          , packPR       = packPR_Double
          , combine2PR   = combine2PR_Double
          , fromListPR   = fromListPR_Double
          , nfPR         = nfPR_Double
          }

{-# INLINE emptyPR_Double #-}
emptyPR_Double = PDouble U.empty

{-# INLINE replicatePR_Double #-}
replicatePR_Double n# i = PDouble (U.replicate (I# n#) i)

{-# INLINE replicatelPR_Double #-}
replicatelPR_Double segd (PDouble xs) = PDouble (U.replicate_s segd xs)

{-# INLINE repeatPR_Double #-}
repeatPR_Double n# len# (PDouble xs) = PDouble (U.repeat (I# n#) (I# len#) xs)

{-# INLINE repeatcPR_Double #-}
repeatcPR_Double n# ns segd (PDouble xs) = PDouble (U.repeat_c (I# n#) ns segd xs)

{-# INLINE indexPR_Double #-}
indexPR_Double (PDouble xs) i# = xs U.!: I# i#

{-# INLINE extractPR_Double #-}
extractPR_Double (PDouble xs) i# n# = PDouble (U.extract xs (I# i#) (I# n#))

bpermutePR_Double :: T_bpermutePR Double
{-# INLINE bpermutePR_Double #-}
bpermutePR_Double (PDouble xs) _ is = PDouble (U.bpermute xs is)

{-# INLINE appPR_Double #-}
appPR_Double (PDouble xs) (PDouble ys) = PDouble (xs U.+:+ ys)

{-# INLINE applPR_Double #-}
applPR_Double xsegd (PDouble xs) ysegd (PDouble ys)
  = PDouble (U.append_s xsegd xs ysegd ys)

packPR_Double :: T_packPR Double
{-# INLINE packPR_Double #-}
packPR_Double (PDouble ns) n# bs = PDouble (U.pack ns bs)

combine2PR_Double :: T_combine2PR Double
{-# INLINE combine2PR_Double #-}
combine2PR_Double n# sel (PDouble xs) (PDouble ys)
  = PDouble (U.combine (U.pick (tagsSel2 sel) 0) xs ys)

fromListPR_Double :: T_fromListPR Double
{-# INLINE fromListPR_Double #-}
fromListPR_Double n# xs = PDouble (U.fromList xs)

{-# INLINE nfPR_Double #-}
nfPR_Double (PDouble xs) = xs `seq` ()


data instance PData Bool
  = PBool Sel2

type instance PRepr Bool = Sum2 Void Void

dPA_Bool :: PA Bool
{-# INLINE_PA dPA_Bool #-}
dPA_Bool = PA {
             toPRepr      = toPRepr_Bool
           , fromPRepr    = fromPRepr_Bool
           , toArrPRepr   = toArrPRepr_Bool
           , fromArrPRepr = fromArrPRepr_Bool
           , dictPRepr    = dPR_Sum2 dPR_Void dPR_Void
           }

{-# INLINE toPRepr_Bool #-}
toPRepr_Bool False = Alt2_1 void
toPRepr_Bool True  = Alt2_2 void

{-# INLINE fromPRepr_Bool #-}
fromPRepr_Bool (Alt2_1 _) = False
fromPRepr_Bool (Alt2_2 _) = True

{-# INLINE toArrPRepr_Bool #-}
toArrPRepr_Bool (PBool sel) = PSum2 sel pvoid pvoid

{-# INLINE fromArrPRepr_Bool #-}
fromArrPRepr_Bool (PSum2 sel _ _) = PBool sel

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

dPA_Unit :: PA ()
{-# INLINE_PA dPA_Unit #-}
dPA_Unit = PA {
             toPRepr      = id
           , fromPRepr    = id
           , toArrPRepr   = id
           , fromArrPRepr = id
           , dictPRepr    = dPR_Unit
           }

type instance PRepr (a,b) = (a,b)

dPA_2 :: PA a -> PA b -> PA (a,b)
{-# INLINE_PA dPA_2 #-}
dPA_2 pa pb = PA {
                toPRepr      = id
              , fromPRepr    = id
              , toArrPRepr   = id
              , fromArrPRepr = id
              , dictPRepr    = dPR_2 (mkPR pa) (mkPR pb)
              }

type instance PRepr (a,b,c) = (a,b,c)

dPA_3 :: PA a -> PA b -> PA c -> PA (a,b,c)
{-# INLINE_PA dPA_3 #-}
dPA_3 pa pb pc
  = PA {
      toPRepr      = id
    , fromPRepr    = id
    , toArrPRepr   = id
    , fromArrPRepr = id
    , dictPRepr    = dPR_3 (mkPR pa) (mkPR pb) (mkPR pc)
    }

type instance PRepr (a,b,c,d) = (a,b,c,d)

dPA_4 :: PA a -> PA b -> PA c -> PA d -> PA (a,b,c,d)
{-# INLINE_PA dPA_4 #-}
dPA_4 pa pb pc pd
  = PA {
      toPRepr      = id
    , fromPRepr    = id
    , toArrPRepr   = id
    , fromArrPRepr = id
    , dictPRepr    = dPR_4 (mkPR pa) (mkPR pb) (mkPR pc) (mkPR pd)
    }

type instance PRepr (a,b,c,d,e) = (a,b,c,d,e)

dPA_5 :: PA a -> PA b -> PA c -> PA d -> PA e -> PA (a,b,c,d,e)
{-# INLINE_PA dPA_5 #-}
dPA_5 pa pb pc pd pe
  = PA {
      toPRepr      = id
    , fromPRepr    = id
    , toArrPRepr   = id
    , fromArrPRepr = id
    , dictPRepr    = dPR_5 (mkPR pa) (mkPR pb) (mkPR pc) (mkPR pd) (mkPR pe)
    }

type instance PRepr (PArray a) = PArray (PRepr a)

dPA_PArray :: PA a -> PA (PArray a)
{-# INLINE_PA dPA_PArray #-}
dPA_PArray pa
  = PA {
      toPRepr      = toPArrayPRepr pa
    , fromPRepr    = fromPArrayPRepr pa
    , toArrPRepr   = toNestedPRepr pa
    , fromArrPRepr = fromNestedPRepr pa
    , dictPRepr    = dPR_PArray (dictPRepr pa)
    }

{-# INLINE toPArrayPRepr #-}
toPArrayPRepr pa (PArray n# xs) = PArray n# (toArrPRepr pa xs)

{-# INLINE fromPArrayPRepr #-}
fromPArrayPRepr pa (PArray n# xs) = PArray n# (fromArrPRepr pa xs)

{-# INLINE toNestedPRepr #-}
toNestedPRepr pa (PNested segd xs) = PNested segd (toArrPRepr pa xs)

{-# INLINE fromNestedPRepr #-}
fromNestedPRepr pa (PNested segd xs) = PNested segd (fromArrPRepr pa xs)

