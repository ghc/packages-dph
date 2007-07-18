module Data.Array.Parallel.Lifted.Instances
where

import Data.Array.Parallel.Lifted.PArray

import GHC.Exts    (Int#)

data instance PArray () = PUnit Int# ()

instance PA () where
  {-# INLINE lengthPA #-}
  lengthPA    (PUnit n _) = n
  {-# INLINE replicatePA #-}
  replicatePA n x         = PUnit n x

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

instance (PA a, PA b) => PA (a,b) where
  {-# INLINE lengthPA #-}
  lengthPA (PTup2 n _ _) = n
  {-# INLINE replicatePA #-}
  replicatePA n p = PTup2 n (p `seq` replicatePA n a)
                            (p `seq` replicatePA n b)
    where
      (a,b) = p

data instance PArray (a,b,c) = PTup3 Int# (PArray a)
                                          (PArray b)
                                          (PArray c)

instance (PA a, PA b, PA c) => PA (a,b,c) where
  {-# INLINE lengthPA #-}
  lengthPA (PTup3 n _ _ _) = n
  {-# INLINE replicatePA #-}
  replicatePA n p = PTup3 n (p `seq` replicatePA n a)
                            (p `seq` replicatePA n b)
                            (p `seq` replicatePA n c)
    where
      (a,b,c) = p

