module Data.Array.Parallel.Lifted.Instances (
  PArray(..),

  dPA_Int, dPR_Int, upToPA_Int,

  dPA_Double, dPR_Double,

  dPA_Bool, toUArrPA_Bool, toPrimArrPA_Bool, truesPA#,
  dPA_Unit, dPA_2, dPA_3,
  dPA_PArray, fromSUArrPA, fromSUArrPA_2
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Unlifted

import GHC.Exts    ( Int#, Int(..), (+#), (-#), (*#),
                     Double#, Double(..) )

data instance PArray Int = PInt Int# PArray_Int#

type instance PRepr Int = Int

instance PrimPA Int where
  fromUArrPA (I# n#) xs          = PInt n# (PInt# xs)
  toUArrPA   (PInt _ (PInt# xs)) = xs
  primPA = dPA_Int

dPA_Int :: PA Int
{-# INLINE dPA_Int #-}
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
          , indexPR      = indexPR_Int
          , bpermutePR   = bpermutePR_Int
          , appPR        = appPR_Int
          , applPR       = applPR_Int
          }

{-# INLINE lengthPR_Int #-}
lengthPR_Int (PInt n# _) = n#

{-# INLINE emptyPR_Int #-}
emptyPR_Int = PInt 0# emptyPA_Int#

{-# INLINE replicatePR_Int #-}
replicatePR_Int n# i = PInt n# (case i of I# i# -> replicatePA_Int# n# i#)

{-# INLINE replicatelPR_Int #-}
replicatelPR_Int n# ns (PInt _ is) = PInt n# (replicatelPA_Int# n# ns is)

{-# INLINE repeatPR_Int #-}
repeatPR_Int n# (PInt m# is) = PInt (n# *# m#) (repeatPA_Int# n# is)

{-# INLINE indexPR_Int #-}
indexPR_Int (PInt _ ns) i# = I# (indexPA_Int# ns i#)

{-# INLINE bpermutePR_Int #-}
bpermutePR_Int (PInt _ ns) is = PInt (lengthPA_Int# is) (bpermutePA_Int# ns is)

{-# INLINE appPR_Int #-}
appPR_Int (PInt m# ms) (PInt n# ns) = PInt (m# +# n#) (appPA_Int# ms ns)

{-# INLINE applPR_Int #-}
applPR_Int is (PInt m# ms) js (PInt n# ns)
  = PInt (m# +# n#) (applPA_Int# is ms js ns)

upToPA_Int :: Int -> PArray Int
{-# INLINE upToPA_Int #-}
upToPA_Int (I# n#) = PInt n# (upToPA_Int# n#)

data instance PArray Double = PDouble Int# PArray_Double#

type instance PRepr Double = Double

instance PrimPA Double where
  fromUArrPA (I# n#) xs                = PDouble n# (PDouble# xs)
  toUArrPA   (PDouble _ (PDouble# xs)) = xs
  primPA = dPA_Double

dPA_Double :: PA Double
{-# INLINE dPA_Double #-}
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
          , indexPR      = indexPR_Double
          , bpermutePR   = bpermutePR_Double
          , appPR        = appPR_Double
          , applPR       = applPR_Double
          }

{-# INLINE lengthPR_Double #-}
lengthPR_Double (PDouble n# _) = n#

{-# INLINE emptyPR_Double #-}
emptyPR_Double = PDouble 0# emptyPA_Double#

{-# INLINE replicatePR_Double #-}
replicatePR_Double n# d
  = PDouble n# (case d of D# d# -> replicatePA_Double# n# d#)

{-# INLINE replicatelPR_Double #-}
replicatelPR_Double n# ns (PDouble _ ds)
  = PDouble n# (replicatelPA_Double# n# ns ds)

{-# INLINE repeatPR_Double #-}
repeatPR_Double n# (PDouble m# is) = PDouble (n# *# m#) (repeatPA_Double# n# is)

{-# INLINE indexPR_Double #-}
indexPR_Double (PDouble _ ds) i# = D# (indexPA_Double# ds i#)

{-# INLINE bpermutePR_Double #-}
bpermutePR_Double (PDouble _ ds) is
  = PDouble (lengthPA_Int# is) (bpermutePA_Double# ds is)

{-# INLINE appPR_Double #-}
appPR_Double (PDouble m# ms) (PDouble n# ns)
  = PDouble (m# +# n#) (appPA_Double# ms ns)

{-# INLINE applPR_Double #-}
applPR_Double is (PDouble m# ms) js (PDouble n# ns)
  = PDouble (m# +# n#) (applPA_Double# is ms js ns)

type instance PRepr Bool = Sum2 Void Void
data instance PArray Bool = PBool Int# PArray_Int# PArray_Int#
                                  (PArray Void) (PArray Void)

dPA_Bool :: PA Bool
{-# INLINE dPA_Bool #-}
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

toUArrPA_Bool :: PArray Bool -> UArr Bool
{-# INLINE toUArrPA_Bool #-}
toUArrPA_Bool (PBool _ (PInt# ns#) _ _ _) = mapU (/= 0) ns#

toPrimArrPA_Bool :: PArray Bool -> PArray_Bool#
{-# INLINE toPrimArrPA_Bool #-}
toPrimArrPA_Bool bs = PBool# (toUArrPA_Bool bs)

instance PrimPA Bool where
  {-# INLINE fromUArrPA #-}
  fromUArrPA (I# n#) bs
    = PBool n#
            (PInt# ts)
            (PInt# is)
            (PVoid (n# -# m#))
            (PVoid m#)
    where
      ts = mapU (\b -> if b then 1 else 0) bs

      is = zipWith3U if_ ts (scanU (+) 0 ts) (scanU (+) 0 $ mapU not_ ts)

      m# = case sumU ts of I# m# -> m#

      {-# INLINE if_ #-}
      if_ 0 x y = y
      if_ _ x y = x

      {-# INLINE not_ #-}
      not_ 0 = 1
      not_ _ = 0

  {-# INLINE toUArrPA #-}
  toUArrPA (PBool _ (PInt# ts) _ _ _) = mapU (/= 0) ts

  primPA = dPA_Bool

truesPA# :: PArray Bool -> Int#
{-# INLINE truesPA# #-}
truesPA# (PBool _ _ _ fs ts) = lengthPA# dPA_Void ts

{-
data instance PArray Bool = PBool Int# PArray_Int# PArray_Int#

type instance PRepr Bool = Enumeration

dPA_Bool :: PA Bool
{-# INLINE dPA_Bool #-}
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
{-# INLINE dPA_Unit #-}
dPA_Unit = PA {
             toPRepr      = id
           , fromPRepr    = id
           , toArrPRepr   = id
           , fromArrPRepr = id
           , dictPRepr    = dPR_Unit
           }

type instance PRepr (a,b) = (a,b)

dPA_2 :: PA a -> PA b -> PA (a,b)
{-# INLINE dPA_2 #-}
dPA_2 pa pb = PA {
                toPRepr      = id
              , fromPRepr    = id
              , toArrPRepr   = id
              , fromArrPRepr = id
              , dictPRepr    = dPR_2 (mkPR pa) (mkPR pb)
              }

type instance PRepr (a,b,c) = (a,b,c)

dPA_3 :: PA a -> PA b -> PA c -> PA (a,b,c)
{-# INLINE dPA_3 #-}
dPA_3 pa pb pc
  = PA {
      toPRepr      = id
    , fromPRepr    = id
    , toArrPRepr   = id
    , fromArrPRepr = id
    , dictPRepr    = dPR_3 (mkPR pa) (mkPR pb) (mkPR pc)
    }

type instance PRepr (PArray a) = PArray (PRepr a)

dPA_PArray :: PA a -> PA (PArray a)
{-# INLINE dPA_PArray #-}
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

