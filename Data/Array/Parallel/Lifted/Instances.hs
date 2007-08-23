module Data.Array.Parallel.Lifted.Instances (
  dPA_Int, dPR_Int, intPayload,
  dPA_Unit, dPA_2, dPA_3
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Unlifted ( UArr, replicateU, emptyU )

import GHC.Exts    (Int#, Int(..))

data instance PArray Int = PInt Int# (UArr Int)

intPayload :: PArray Int -> UArr Int
{-# INLINE intPayload #-}
intPayload (PInt _ is) = is

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
emptyPR_Int = PInt 0# emptyU

{-# INLINE replicatePR_Int #-}
replicatePR_Int n# i = PInt n# (replicateU (I# n#) i)

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

type instance PRepr (a,b) = (Embed a, Embed b)

dPA_2 :: PA a -> PA b -> PA (a,b)
{-# INLINE dPA_2 #-}
dPA_2 pa pb = PA {
                toPRepr      = toPRepr_2
              , fromPRepr    = fromPRepr_2
              , toArrPRepr   = toArrPRepr_2
              , fromArrPRepr = fromArrPRepr_2
              , dictPRepr    = pr
              }
  where
    pr = dPR_2 (dPR_Embed pa) (dPR_Embed pb)

toPRepr_2 :: (a, b) -> PRepr (a, b)
{-# INLINE toPRepr_2 #-}
toPRepr_2 (a, b) = (Embed a, Embed b)

-- FIXME: fromPRepr_2 :: PRepr (a, b) -> (a, b)
-- doesn't work atm
fromPRepr_2 :: (Embed a, Embed b) -> (a, b)
{-# INLINE fromPRepr_2 #-}
fromPRepr_2 (Embed a, Embed b) = (a, b)

toArrPRepr_2 :: PArray (a, b) -> PArray (PRepr (a, b))
{-# INLINE toArrPRepr_2 #-}
toArrPRepr_2 (P_2 n# as bs) = P_2 n# (PEmbed as) (PEmbed bs)

fromArrPRepr_2 :: PArray (Embed a, Embed b) -> PArray (a, b)
{-# INLINE fromArrPRepr_2 #-}
fromArrPRepr_2 (P_2 n# (PEmbed as) (PEmbed bs)) = P_2 n# as bs

type instance PRepr (a,b,c) = (Embed a, Embed b, Embed c)

dPA_3 :: PA a -> PA b -> PA c -> PA (a,b,c)
{-# INLINE dPA_3 #-}
dPA_3 pa pb pc
  = PA {
      toPRepr      = toPRepr_3
    , fromPRepr    = fromPRepr_3
    , toArrPRepr   = toArrPRepr_3
    , fromArrPRepr = fromArrPRepr_3
    , dictPRepr    = pr
    }
  where
    pr = dPR_3 (dPR_Embed pa)
               (dPR_Embed pb)
               (dPR_Embed pc)


toPRepr_3 :: (a, b, c) -> PRepr (a, b, c)
{-# INLINE toPRepr_3 #-}
toPRepr_3 (a, b, c) = (Embed a, Embed b, Embed c)

-- FIXME
fromPRepr_3 :: (Embed a, Embed b, Embed c) -> (a, b, c)
{-# INLINE fromPRepr_3 #-}
fromPRepr_3 (Embed a, Embed b, Embed c) = (a, b, c)

toArrPRepr_3 :: PArray (a, b, c) -> PArray (PRepr (a, b, c))
{-# INLINE toArrPRepr_3 #-}
toArrPRepr_3 (P_3 n# as bs cs)
  = P_3 n# (PEmbed as) (PEmbed bs) (PEmbed cs)

fromArrPRepr_3 :: PArray (Embed a, Embed b, Embed c)
               -> PArray (a, b, c)
{-# INLINE fromArrPRepr_3 #-}
fromArrPRepr_3 (P_3 n# (PEmbed as) (PEmbed bs) (PEmbed cs)) = P_3 n# as bs cs

