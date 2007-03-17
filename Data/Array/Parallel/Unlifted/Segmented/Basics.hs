-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Basics
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
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
  lengthSU, replicateSU,
  flattenSU, (>:), segmentU, concatSU,
  sliceIndexSU, extractIndexSU,
  fstSU, sndSU, zipSU,
  enumFromToSU, enumFromThenToSU,
  toSU, fromSU
) where

import Data.Array.Parallel.Base (
  (:*:)(..))
import Data.Array.Parallel.Stream (
  Step(..), Stream(..),
  replicateEachS, zipS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr,
  (!:), sliceU, extractU,
  mapU, fstU, sndU, zipU, zipWith3U, sumU,
  toU, fromU,
  streamU, unstreamU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, lengthSU, (>:), flattenSU, segdSU, lengthsSU, indicesSU,
  lengthsToUSegd)

-- lengthSU reexported from SUArr

-- |Segmentation
-- -------------

-- flattenSU and (>:) reexported from SUArr

replicateSU :: UA e => UArr Int -> UArr e -> SUArr e
{-# INLINE replicateSU #-}
replicateSU ns es =
  lengthsToUSegd ns >: unstreamU (replicateEachS (sumU ns)
                                                 (zipS (streamU ns)
                                                       (streamU es)))

-- |Segment an array according to the segmentation of the first argument
--
segmentU :: (UA e', UA e) => SUArr e' -> UArr e -> SUArr e
{-# INLINE segmentU #-}
segmentU template arr = segdSU template >: arr

-- |Concatenate the subarrays of an array of arrays
--
concatSU :: UA e => SUArr e -> UArr e
concatSU = flattenSU

-- |Indexing
-- ---------

-- |Extract the segment at the given index using the given extraction function
-- (either 'sliceU' or 'extractU').
-- 
indexSU :: UA e => (UArr e -> Int -> Int -> UArr e) -> SUArr e -> Int -> UArr e
indexSU copy sa i = copy (concatSU sa) (indicesSU sa !: i)
                                       (lengthsSU sa !: i)

-- |Extract the segment at the given index without copying the elements.
--
sliceIndexSU :: UA e => SUArr e -> Int -> UArr e
sliceIndexSU = indexSU sliceU

-- |Extract the segment at the given index (elements are copied).
-- 
extractIndexSU :: UA e => SUArr e -> Int -> UArr e
extractIndexSU = indexSU extractU

-- |Zipping
-- --------

fstSU :: (UA a, UA b) => SUArr (a :*: b) -> SUArr a
{-# INLINE fstSU #-}
fstSU sa = segdSU sa >: fstU (concatSU sa)

sndSU :: (UA a, UA b) => SUArr (a :*: b) -> SUArr b
{-# INLINE sndSU #-}
sndSU sa = segdSU sa >: sndU (concatSU sa)

zipSU :: (UA a, UA b) => SUArr a -> SUArr b -> SUArr (a :*: b)
{-# INLINE zipSU #-}
zipSU sa sb = segdSU sa >: zipU (concatSU sa) (concatSU sb)

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
  segd >: unstreamU (enumFromThenToEachS len
                    (streamU (zipU (zipU lens starts) nexts)))
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
    len     = sumU lens
    segd    = lengthsToUSegd lens

enumFromThenToEachS :: Enum a => Int -> Stream (Int :*: a :*: a) -> Stream a
{-# INLINE [1] enumFromThenToEachS #-}
enumFromThenToEachS n (Stream next s _) = 
  Stream next' (0 :*: 0 :*: 0 :*: s) n
  where
    next' (0 :*: start :*: delta :*: s) =
      case next s of
        Done    -> Done
        Skip s' -> Skip (0 :*: start :*: delta :*: s')
        Yield (len :*: i :*: k) s'
                -> let start' = fromEnum i
                   in Skip (len :*: start' :*: fromEnum k - start' :*: s')
    next' (n :*: start :*: delta :*: s) =
      Yield (toEnum start) (n-1 :*: start+delta :*: delta :*: s)

-- |Conversion
-- -----------

-- |Turn a nested list into a segmented parallel array
--
toSU :: UA e => [[e]] -> SUArr e
{-# INLINE toSU #-}
toSU ls = let lens = toU $ map length ls
	      a    = toU $ concat ls
          in
	  lengthsToUSegd lens >: a

-- |Turn a segmented array into a nested list
--
fromSU :: UA e => SUArr e -> [[e]]
{-# INLINE fromSU #-}
fromSU sa = let a       = concatSU sa
                lens    = fromU $ lengthsSU sa
                starts  = fromU $ indicesSU sa
            in
            [[a !: i | i <- [start .. start + len - 1]]
                            | (start, len) <- zip starts lens]

