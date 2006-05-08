-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Basics
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
--  Basics operations on unlifted segmented arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Basics (
  lengthSU,
  flattenSU, (>:), segmentU, concatSU,
  sliceIndexSU, extractIndexSU,
  enumFromToSU, enumFromThenToSU,
  toSU, fromSU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr,
  lengthU, (!:), replicateU,
  sliceU, extractU,
  mapU, zipWith3U, sumU,
  toU, fromU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr(..), lengthSU, (>:), flattenSU, psumUS, segdUS, toUSegd)
import Data.Array.Parallel.Unlifted.Segmented.Loop (
  loopSU)
import Data.Array.Parallel.Unlifted.Segmented.Fusion (
  loopArrS)

-- lengthSU reexported from SUArr

-- |Segmentation
-- -------------

-- flattenSU and (>:) reexported from SUArr

-- |Segment an array according to the segmentation of the first argument
--
segmentU :: (UA e', UA e) => SUArr e' -> UArr e -> SUArr e
{-# INLINE segmentU #-}
segmentU template arr = fstS (flattenSU template) >: arr

-- |Concatenate the subarrays of an array of arrays
--
concatSU :: UA e => SUArr e -> UArr e
concatSU = sndS . flattenSU

-- |Indexing
-- ---------

-- |Extract the segment at the given index using the given extraction function
-- (either 'sliceU' or 'extractU').
-- 
indexSU :: UA e => (UArr e -> Int -> Int -> UArr e) -> SUArr e -> Int -> UArr e
indexSU copy (SUArr segd a) i = copy a (psumUS segd !: i)
                                       (segdUS segd !: i)

-- |Extract the segment at the given index without copying the elements.
--
sliceIndexSU :: UA e => SUArr e -> Int -> UArr e
sliceIndexSU = indexSU sliceU

-- |Extract the segment at the given index (elements are copied).
-- 
extractIndexSU :: UA e => SUArr e -> Int -> UArr e
extractIndexSU = indexSU extractU


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

-- |Conversion
-- -----------

-- |Turn a nested list into a segmented parallel array
--
toSU :: UA e => [[e]] -> SUArr e
{-# INLINE toSU #-}
toSU ls = let lens = toU $ map length ls
	      a    = toU $ concat ls
          in
	  toUSegd lens >: a

-- |Turn a segmented array into a nested list
--
fromSU :: UA e => SUArr e -> [[e]]
{-# INLINE fromSU #-}
fromSU as = let (segd :*: a) = flattenSU as
                lens         = fromU $ segdUS segd
                starts       = fromU $ psumUS segd
            in
            [[a !: i | i <- [start .. start + len - 1]]
                            | (start, len) <- zip starts lens]

