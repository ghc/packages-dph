-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.ListLike
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Unlifted array versions of segmented list-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.ListLike (
  concatSU, {-concatMapU,-},
  foldlSU, foldSU, {-fold1SU,-} {-scanSU,-} {-scan1SU,-}
  andSU, orSU, sumSU, productSU, maximumSU, minimumSU,
  enumFromToSU, enumFromThenToSU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr,
  (!:), lengthU, mapU, replicateU, zipWith3U, sumU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, toUSegd, flattenSU, (>:))
import Data.Array.Parallel.Unlifted.Segmented.Loop (
  loopSU)
import Data.Array.Parallel.Unlifted.Segmented.Fusion (
  foldEFL, keepSFL,
  loopAccS, loopArrS)

-- |List-like combinators
-- ----------------------

-- |Concatenate the subarrays of an array of arrays
--
concatSU :: UA e => SUArr e -> UArr e
concatSU = sndS . flattenSU

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSU #-}
foldlSU f z = loopAccS . loopSU (foldEFL f) (keepSFL (const z)) z

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

-- |
andSU :: SUArr Bool -> UArr Bool
andSU = foldSU (&&) True

-- |
orSU :: SUArr Bool -> UArr Bool
orSU = foldSU (||) False

-- |Compute the segmented sum of an array of numerals
--
sumSU :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE sumSU #-}
sumSU = foldSU (+) 0

-- |Compute the segmented product of an array of numerals
--
productSU :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE productSU #-}
productSU = foldSU (*) 1

-- |Determine the maximum element in each subarray
--
maximumSU :: (Bounded e, Ord e, UA e) => SUArr e -> UArr e
--FIXME: provisional until fold1SU implemented
--maximumSU :: (Ord e, MUA e) => UArr (UArr e) -> UArr e
{-# INLINE maximumSU #-}
--maximumSU = fold1SU max
maximumSU = foldSU max minBound

-- |Determine the minimum element in each subarray
--
minimumSU :: (Bounded e, Ord e, UA e) => SUArr e -> UArr e
--FIXME: provisional until fold1SU implemented
--minimumSU :: (Ord e, MUA e) => UArr (UArr e) -> UArr e
{-# INLINE minimumSU #-}
--minimumSU = fold1SU min
minimumSU = foldSU min maxBound

-- |Enumeration functions
-- ----------------------

-- |Yield a segmented enumerated array
--
enumFromToSU :: (Enum e, UA e) => UArr e -> UArr e -> SUArr e
{-# INLINE enumFromToSU #-}
enumFromToSU starts = enumFromThenToSU starts (mapU succ starts)

-- |Yield a segmented enumerated array using a specific step
--
enumFromThenToSU :: (Enum e, UA e) 
		 => UArr e -> UArr e -> UArr e -> SUArr e
{-# INLINE enumFromThenToSU #-}
enumFromThenToSU starts nexts ends = 
  loopArrS $ loopSU step seg init (segd >: replicateU len ())
  where
    lens    = zipWith3U calcLen starts nexts ends
	      where
		calcLen start next end = 
		  abs (end' - start' + delta) `div` (abs delta)
		  where
		    start' = fromEnum start
		    next'  = fromEnum next
		    end'   = fromEnum end
		    delta  = next' - start'
    len     = sumU    lens
    segd    = toUSegd lens
    segdlen = lengthU lens
    --
    step (x :*: delta) _ = ((x + delta :*: delta) :*: (Just $ toEnum x))
    seg  _             i = ((start :*: delta) :*: (Nothing::Maybe ()))
			   where
			     start = fromEnum (starts!:(i + 1))
			     next  = fromEnum (nexts !:(i + 1))
			     delta = if (i + 1) == segdlen 
				     then 0 
				     else next - start
    --
    init = fstS $ seg undefined (-1)

