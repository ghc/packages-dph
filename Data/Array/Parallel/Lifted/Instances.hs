module Data.Array.Parallel.Lifted.Instances (
  dPA_Int, intPayload,
  dPA_0, dPA_2, dPA_3
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Unlifted ( UArr, replicateU )

import GHC.Exts    (Int#, Int(..))

data instance PArray Int = PInt Int# !(UArr Int)

intPayload :: PArray Int -> UArr Int
{-# INLINE intPayload #-}
intPayload (PInt _ is) = is

type instance PRepr Int = Int

dPA_Int :: PA Int
{-# INLINE dPA_Int #-}
dPA_Int = PA {
            lengthPA    = lengthPA_Int
          , replicatePA = replicatePA_Int
          , toPRepr     = id
          , fromPRepr   = id
          }

lengthPA_Int :: PArray Int -> Int#
{-# INLINE lengthPA_Int #-}
lengthPA_Int (PInt n _) = n

replicatePA_Int :: Int# -> Int -> PArray Int
{-# INLINE replicatePA_Int #-}
replicatePA_Int n i = PInt n (replicateU (I# n) i)

data instance PArray () = PUnit Int# ()

type instance PRepr () = ()

dPA_0 :: PA ()
{-# INLINE dPA_0 #-}
dPA_0 = PA {
             lengthPA    = lengthPA_0
           , replicatePA = replicatePA_0
           , toPRepr     = id
           , fromPRepr   = id
           }

lengthPA_0 :: PArray () -> Int#
{-# INLINE lengthPA_0 #-}
lengthPA_0 (PUnit n# _) = n#

replicatePA_0 :: Int# -> () -> PArray ()
{-# INLINE replicatePA_0 #-}
replicatePA_0 = PUnit

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

data instance PArray (a,b) = PTup2 Int# (PArray a) (PArray b)

type instance PRepr (a,b) = Embed a :*: Embed b

dPA_2 :: PA a -> PA b -> PA (a,b)
{-# INLINE dPA_2 #-}
dPA_2 pa pb = PA {
                lengthPA    = lengthPA_2
              , replicatePA = replicatePA_2 pa pb
              , toPRepr     = toPRepr_2 pa pb
              , fromPRepr   = fromPRepr_2
              }

lengthPA_2 :: PArray (a,b) -> Int#
{-# INLINE lengthPA_2 #-}
lengthPA_2 (PTup2 n# _ _) = n#

replicatePA_2 :: PA a -> PA b -> Int# -> (a,b) -> PArray (a,b)
{-# INLINE replicatePA_2 #-}
replicatePA_2 pa pb n# p = PTup2 n# (p `seq` replicatePA pa n# a)
                                    (p `seq` replicatePA pb n# b) 
  where (a,b) = p

toPRepr_2 :: PA a -> PA b -> (a, b) -> PRepr (a, b)
{-# INLINE toPRepr_2 #-}
toPRepr_2 pa pb (a, b) = Embed pa a :*: Embed pb b

-- FIXME: fromPRepr_2 :: PRepr (a, b) -> (a, b)
-- doesn't work atm
fromPRepr_2 :: Embed a :*: Embed b -> (a, b)
{-# INLINE fromPRepr_2 #-}
fromPRepr_2 (Embed _ a :*: Embed _ b) = (a, b)

data instance PArray (a,b,c) = PTup3 Int# (PArray a)
                                          (PArray b)
                                          (PArray c)

type instance PRepr (a,b,c) = Embed a :*: Embed b :*: Embed c

dPA_3 :: PA a -> PA b -> PA c -> PA (a,b,c)
{-# INLINE dPA_3 #-}
dPA_3 pa pb pc
  = PA {
      lengthPA    = lengthPA_3
    , replicatePA = replicatePA_3 pa pb pc
    , toPRepr     = toPRepr_3 pa pb pc
    , fromPRepr   = fromPRepr_3
    }

lengthPA_3 :: PArray (a,b,c) -> Int#
{-# INLINE lengthPA_3 #-}
lengthPA_3 (PTup3 n# _ _ _) = n#

replicatePA_3 :: PA a -> PA b -> PA c
              -> Int# -> (a,b,c) -> PArray (a,b,c)
{-# INLINE replicatePA_3 #-}
replicatePA_3 pa pb pc n# p
  = PTup3 n# (p `seq` replicatePA pa n# a)
             (p `seq` replicatePA pb n# b) 
             (p `seq` replicatePA pc n# c)
  where (a,b,c) = p

toPRepr_3 :: PA a -> PA b -> PA c -> (a, b, c) -> PRepr (a, b, c)
{-# INLINE toPRepr_3 #-}
toPRepr_3 pa pb pc (a, b, c) = Embed pa a :*: Embed pb b :*: Embed pc c

-- FIXME
fromPRepr_3 :: Embed a :*: Embed b :*: Embed c -> (a, b, c)
{-# INLINE fromPRepr_3 #-}
fromPRepr_3 (Embed _ a :*: Embed _ b :*: Embed _ c) = (a, b, c)

