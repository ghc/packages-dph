-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Base.Fusion
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (unboxed values and GHC libraries)
--
-- Description ---------------------------------------------------------------
--
-- Utilities for fusion combinators
--

module Data.Array.Parallel.Base.Fusion (
  NoAL, noAL,
  fuseEFL,
  noEFL, noSFL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL,
  loopArr, loopAcc, loopSndAcc, loopArrS, loopAccS,

  -- * Strict pairs (reexported)
  (:*:)(..))
where

import Data.Array.Parallel.Base.Hyperstrict

infixr 9 `fuseEFL`

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAL) `seq` x = x@.
--
data NoAL = NoAL

-- |No accumulator
--
noAL :: NoAL
{-# INLINE [1] noAL #-}
noAL = NoAL

-- |Fusion of loop functions
-- -------------------------

-- |Fuse to flat loop functions
fuseEFL :: (acc1 -> e1 -> acc1 :*: Maybe e2)
        -> (acc2 -> e2 -> acc2 :*: Maybe e3)
        -> acc1 :*: acc2 -> e1 -> (acc1 :*: acc2) :*: Maybe e3
fuseEFL f g (acc1 :*: acc2) e1 =
  case f acc1 e1 of
    acc1' :*: Nothing -> (acc1' :*: acc2) :*: Nothing
    acc1' :*: Just e2 ->
      case g acc2 e2 of
        (acc2' :*: res) -> (acc1' :*: acc2') :*: res

-- |Special forms of loop arguments
-- --------------------------------
--
-- * These are common special cases for the three function arguments of gen
--   and loop; we give them special names to make it easier to trigger RULES
--   applying in the special cases represented by these arguments.  The
--   "INLINE [1]" makes sure that these functions are only inlined in the last
--   two simplifier phases.
--
-- * In the case where the accumulator is not needed, it is better to always
--   explicitly return a value `()', rather than just copy the input to the
--   output, as the former gives GHC better local information.
-- 

-- |No element function
--
noEFL :: acc -> () -> (acc :*: Maybe ())
{-# INLINE [1] noEFL #-}
noEFL acc _  = (acc :*: Nothing)

-- |No segment function
--
noSFL :: acc -> Int -> (acc :*: Maybe ())
{-# INLINE [1] noSFL #-}
noSFL acc _  = (acc :*: Nothing)

-- |Element function expressing a mapping only
--
mapEFL :: (e -> e') -> (NoAL -> e -> (NoAL :*: Maybe e'))
{-# INLINE [1] mapEFL #-}
mapEFL f = \a e -> (noAL :*: (Just $ f e))
--mapEFL f = \a e -> let e' = f e in e' `seq` (noAL :*: Just e')

-- |Element function implementing a filter function only
--
filterEFL :: (e -> Bool) -> (NoAL -> e -> (NoAL :*: Maybe e))
{-# INLINE [1] filterEFL #-}
filterEFL p = \a e -> if p e then (noAL :*: Just e) else (noAL :*: Nothing)

-- |Element function expressing a reduction only
--
foldEFL :: (acc -> e -> acc) -> (acc -> e -> (acc :*: Maybe ()))
{-# INLINE [1] foldEFL #-}
foldEFL f = \a e -> (f a e :*: Nothing)
--foldEFL f = \a e -> let a' = f a e in a' `seq` (a' :*: Nothing)

-- |Element function expressing a prefix reduction only
--
scanEFL :: (acc -> e -> acc) -> (acc -> e -> (acc :*: Maybe acc))
{-# INLINE [1] scanEFL #-}
scanEFL f = \a e -> (f a e :*: Just a)
--scanEFL f  = \a e -> let a' = f a e in a' `seq` (a' :*: Just a)

-- |Segment function transforming the accumulator
--
transSFL :: (acc -> acc) -> (acc -> Int -> (acc :*: Maybe ()))
{-# INLINE [1] transSFL #-}
transSFL f = \a _ -> (f a :*: Nothing)

-- |Segment function transforming the accumulator and collecting accumulator
-- values
--
keepSFL :: (acc -> acc) -> (acc -> Int -> (acc :*: Maybe acc))
{-# INLINE [1] keepSFL #-}
keepSFL f = \a _ -> (f a :*: Just a)

-- Fusion-friendly projection functions
-- ------------------------------------
--
-- We determine when they are inlined

-- |Array result of a loop
--
loopArr :: (arr :*: acc) -> arr
{-# INLINE [1] loopArr #-}
loopArr (arr :*: _) = arr

-- |Accumulator result of a loop
--
loopAcc :: (arr :*: acc) -> acc
{-# INLINE [1] loopAcc #-}
loopAcc (_ :*: acc) = acc

-- |Drop the first component of the accumulator
--
loopSndAcc :: (arr :*: (acc1 :*: acc2)) -> (arr :*: acc2)
{-# INLINE [1] loopSndAcc #-}
loopSndAcc (arr :*: (_ :*: acc)) = (arr :*: acc)

loopArrS :: ((arr :*: accs) :*: acc) -> arr
{-# INLINE [1] loopArrS #-}
loopArrS ((arr :*: _) :*: _) = arr

loopAccS :: ((arr :*: accs) :*: acc) -> accs
{-# INLINE [1] loopAccS #-}
loopAccS ((_ :*: accs) :*: _) = accs

{-# RULES  -- -} (for font-locking)

"loopArr/loopSndAcc" forall x.
  loopArr (loopSndAcc x) = loopArr x

--FIXME: there are zip variants, too

"loopArrS/loopSndAcc" forall x.
  loopArrS (loopSndAcc x) = loopArrS x

"loopAccS/loopSndAcc" forall x.
  loopAccS (loopSndAcc x) = loopAccS x

-- Ignore `seq's on the `NoAL' type.  This is essentially to ensure that no
-- superflous demands are introduced by accumulators that are not used.
--
"seq/NoAL" forall (u::NoAL) e.
  u `seq` e = e

 #-}

