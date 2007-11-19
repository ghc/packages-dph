module Data.Array.Parallel.Lifted.Instances (
  PArray(..),
  dPA_Int, dPR_Int,
  dPA_Bool,
  dPA_Unit, dPA_2, dPA_3,
  dPA_PArray
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Unlifted ( UArr, replicateU, emptyU )

import GHC.Exts    (Int#, Int(..))

data instance PArray Int = PInt Int# PArray_Int#

type instance PRepr Int = Int

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
            lengthPR    = lengthPR_Int
          , emptyPR     = emptyPR_Int
          , replicatePR = replicatePR_Int
          }

{-# INLINE lengthPR_Int #-}
lengthPR_Int (PInt n# _) = n#

{-# INLINE emptyPR_Int #-}
emptyPR_Int = PInt 0# emptyPA_Int#

{-# INLINE replicatePR_Int #-}
replicatePR_Int n# i = PInt n# (case i of I# i# -> replicatePA_Int# n# i#)

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

