{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Instances (
  PArray(..),

  dPA_Int, dPR_Int, upToPA_Int,

  dPA_Word8, dPR_Word8,
  dPA_Double, dPR_Double,

  dPA_Bool, toPrimArrPA_Bool, truesPA#,
  dPA_Unit, dPA_2, dPA_3, dPA_4, dPA_5,
  dPA_PArray
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Unboxed

import GHC.Exts    ( Int#, Int(..), (+#), (*#),
                     Double#, Double(..) )
import GHC.Word    ( Word8(..) )

data instance PArray Int = PInt Int# PArray_Int#

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
            lengthPR     = lengthPR_Int
          , emptyPR      = emptyPR_Int
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

{-# INLINE lengthPR_Int #-}
lengthPR_Int (PInt n# _) = n#

{-# INLINE emptyPR_Int #-}
emptyPR_Int = PInt 0# emptyPA_Int#

{-# INLINE replicatePR_Int #-}
replicatePR_Int n# i = PInt n# (case i of I# i# -> replicatePA_Int# n# i#)

{-# INLINE replicatelPR_Int #-}
replicatelPR_Int segd (PInt _ is) = PInt (elementsSegdPA# segd)
                                         (replicatelPA_Int# segd is)

{-# INLINE repeatPR_Int #-}
repeatPR_Int n# len# (PInt _ is) = PInt (n# *# len#) (repeatPA_Int# n# len# is)

{-# INLINE repeatcPR_Int #-}
repeatcPR_Int n# ns segd (PInt _ is)
  = PInt n# (repeatcPA_Int# n# ns segd is)

{-# INLINE indexPR_Int #-}
indexPR_Int (PInt _ ns) i# = I# (indexPA_Int# ns i#)

{-# INLINE extractPR_Int #-}
extractPR_Int (PInt _ ns) i# n# = PInt n# (extractPA_Int# ns i# n#)

{-# INLINE bpermutePR_Int #-}
bpermutePR_Int n# (PInt _ ns) is = PInt n# (bpermutePA_Int# ns is)

{-# INLINE appPR_Int #-}
appPR_Int (PInt m# ms) (PInt n# ns) = PInt (m# +# n#) (appPA_Int# ms ns)

{-# INLINE applPR_Int #-}
applPR_Int is (PInt m# ms) js (PInt n# ns)
  = PInt (m# +# n#) (applPA_Int# is ms js ns)

{-# INLINE packPR_Int #-}
packPR_Int (PInt _ ns) n# bs = PInt n# (packPA_Int# ns n# bs)

{-# INLINE combine2PR_Int #-}
combine2PR_Int n# sel is (PInt _ xs) (PInt _ ys)
  = PInt n# (combine2PA_Int# n# sel is xs ys)

{-# INLINE fromListPR_Int #-}
fromListPR_Int n# xs = PInt n# (fromListPA_Int# n# xs)

{-# INLINE nfPR_Int #-}
nfPR_Int (PInt _ xs) = xs `seq` ()

upToPA_Int :: Int -> PArray Int
{-# INLINE_PA upToPA_Int #-}
upToPA_Int (I# n#) = PInt n# (upToPA_Int# n#)

data instance PArray Word8 = PWord8 Int# PArray_Word8#

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
            lengthPR     = lengthPR_Word8
          , emptyPR      = emptyPR_Word8
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

{-# INLINE lengthPR_Word8 #-}
lengthPR_Word8 (PWord8 n# _) = n#

{-# INLINE emptyPR_Word8 #-}
emptyPR_Word8 = PWord8 0# emptyPA_Word8#

{-# INLINE replicatePR_Word8 #-}
replicatePR_Word8 n# d
  = PWord8 n# (case d of W8# d# -> replicatePA_Word8# n# d#)

{-# INLINE replicatelPR_Word8 #-}
replicatelPR_Word8 segd (PWord8 _ ds)
  = PWord8 (elementsSegdPA# segd) (replicatelPA_Word8# segd ds)

{-# INLINE repeatPR_Word8 #-}
repeatPR_Word8 n# len# (PWord8 _ is)
  = PWord8 (n# *# len#) (repeatPA_Word8# n# len# is)

{-# INLINE repeatcPR_Word8 #-}
repeatcPR_Word8 n# ns segd (PWord8 _ is)
  = PWord8 n# (repeatcPA_Word8# n# ns segd is)

{-# INLINE indexPR_Word8 #-}
indexPR_Word8 (PWord8 _ ds) i# = W8# (indexPA_Word8# ds i#)

{-# INLINE extractPR_Word8 #-}
extractPR_Word8 (PWord8 _ ns) i# n# = PWord8 n# (extractPA_Word8# ns i# n#)

{-# INLINE bpermutePR_Word8 #-}
bpermutePR_Word8 n# (PWord8 _ ds) is
  = PWord8 n# (bpermutePA_Word8# ds is)

{-# INLINE appPR_Word8 #-}
appPR_Word8 (PWord8 m# ms) (PWord8 n# ns)
  = PWord8 (m# +# n#) (appPA_Word8# ms ns)

{-# INLINE applPR_Word8 #-}
applPR_Word8 is (PWord8 m# ms) js (PWord8 n# ns)
  = PWord8 (m# +# n#) (applPA_Word8# is ms js ns)

{-# INLINE packPR_Word8 #-}
packPR_Word8 (PWord8 _ ns) n# bs = PWord8 n# (packPA_Word8# ns n# bs)

{-# INLINE combine2PR_Word8 #-}
combine2PR_Word8 n# sel is (PWord8 _ xs) (PWord8 _ ys)
  = PWord8 n# (combine2PA_Word8# n# sel is xs ys)

{-# INLINE fromListPR_Word8 #-}
fromListPR_Word8 n# xs = PWord8 n# (fromListPA_Word8# n# xs)

{-# INLINE nfPR_Word8 #-}
nfPR_Word8 (PWord8 _ xs) = xs `seq` ()

data instance PArray Double = PDouble Int# PArray_Double#

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
            lengthPR     = lengthPR_Double
          , emptyPR      = emptyPR_Double
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

{-# INLINE lengthPR_Double #-}
lengthPR_Double (PDouble n# _) = n#

{-# INLINE emptyPR_Double #-}
emptyPR_Double = PDouble 0# emptyPA_Double#

{-# INLINE replicatePR_Double #-}
replicatePR_Double n# d
  = PDouble n# (case d of D# d# -> replicatePA_Double# n# d#)

{-# INLINE replicatelPR_Double #-}
replicatelPR_Double segd (PDouble _ ds)
  = PDouble (elementsSegdPA# segd) (replicatelPA_Double# segd ds)

{-# INLINE repeatPR_Double #-}
repeatPR_Double n# len# (PDouble _ is)
  = PDouble (n# *# len#) (repeatPA_Double# n# len# is)

{-# INLINE repeatcPR_Double #-}
repeatcPR_Double n# ns segd (PDouble _ is)
  = PDouble n# (repeatcPA_Double# n# ns segd is)

{-# INLINE indexPR_Double #-}
indexPR_Double (PDouble _ ds) i# = D# (indexPA_Double# ds i#)

{-# INLINE extractPR_Double #-}
extractPR_Double (PDouble _ ns) i# n#
  = PDouble n# (extractPA_Double# ns i# n#)

{-# INLINE bpermutePR_Double #-}
bpermutePR_Double n# (PDouble _ ds) is
  = PDouble n# (bpermutePA_Double# ds is)

{-# INLINE appPR_Double #-}
appPR_Double (PDouble m# ms) (PDouble n# ns)
  = PDouble (m# +# n#) (appPA_Double# ms ns)

{-# INLINE applPR_Double #-}
applPR_Double is (PDouble m# ms) js (PDouble n# ns)
  = PDouble (m# +# n#) (applPA_Double# is ms js ns)

{-# INLINE packPR_Double #-}
packPR_Double (PDouble _ ns) n# bs = PDouble n# (packPA_Double# ns n# bs)

{-# INLINE combine2PR_Double #-}
combine2PR_Double n# sel is (PDouble _ xs) (PDouble _ ys)
  = PDouble n# (combine2PA_Double# n# sel is xs ys)

{-# INLINE fromListPR_Double #-}
fromListPR_Double n# xs = PDouble n# (fromListPA_Double# n# xs)

{-# INLINE nfPR_Double #-}
nfPR_Double (PDouble _ xs) = xs `seq` ()

type instance PRepr Bool = Sum2 Void Void
data instance PArray Bool = PBool Int# PArray_Int# PArray_Int#
                                  (PArray Void) (PArray Void)

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
toArrPRepr_Bool (PBool n# sel# is# fs ts) = PSum2 n# sel# is# fs ts

{-# INLINE fromArrPRepr_Bool #-}
fromArrPRepr_Bool (PSum2 n# sel# is# fs ts) = PBool n# sel# is# fs ts

toPrimArrPA_Bool :: PArray Bool -> PArray_Bool#
{-# INLINE toPrimArrPA_Bool #-}
toPrimArrPA_Bool (PBool _ ns# _ _ _) = toBoolPA# ns#

truesPA# :: PArray Bool -> Int#
{-# INLINE_PA truesPA# #-}
truesPA# (PBool _ _ _ fs ts) = lengthPA# dPA_Void ts

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
      toPRepr      = toArrPRepr pa
    , fromPRepr    = fromArrPRepr pa
    , toArrPRepr   = toNestedPRepr pa
    , fromArrPRepr = fromNestedPRepr pa
    , dictPRepr    = dPR_PArray (dictPRepr pa)
    }

{-# INLINE toNestedPRepr #-}
toNestedPRepr pa (PNested n# lens is xs)
  = PNested n# lens is (toArrPRepr pa xs)

{-# INLINE fromNestedPRepr #-}
fromNestedPRepr pa (PNested n# lens is xs)
  = PNested n# lens is (fromArrPRepr pa xs)

