-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics
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

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics (
  lengthSU, singletonSU, singletonsSU, replicateSU, replicateCU, repeatCU,
  (!:^), concatSU, (>:), segmentU, segmentArrU, appendSU, (^+:+^),
  sliceIndexSU, extractIndexSU, indexedSU,
  fstSU, sndSU, zipSU,
  enumFromToSU, enumFromThenToSU,
  toSU, fromSU,(+:+^)
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..))
import Data.Array.Parallel.Stream (
  Step(..), Stream(..),SStream(..),(+++), (^+++^), zip3S)
import Data.Array.Parallel.Unlifted.Sequential.Flat


import Data.Array.Parallel.Unlifted.Sequential.Segmented.Stream (
  streamSU,unstreamSU)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.SUArr (
  SUArr, USegd, lengthSU, (>:), concatSU, segdSU, lengthsSU, indicesSU,
  lengthsToUSegd, singletonUSegd, toUSegd, fromUSegd)

-- TODO: Remove
import Debug.Trace
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (lengthsUSegd, indicesUSegd)

-- lengthSU reexported from SUArr

-- |Segmentation
-- -------------

-- concatSU and (>:) reexported from SUArr

singletonSU :: UA e => UArr e -> SUArr e
{-# INLINE_U singletonSU #-}
singletonSU es = singletonUSegd (lengthU es) >: es

singletonsSU :: UA e => UArr e -> SUArr e
{-# INLINE_U singletonsSU #-}
singletonsSU es = toUSegd (zipU (replicateU n 1) (enumFromToU 0 (n-1))) >: es
  where
    n = lengthU es

replicateSU :: UA e => USegd -> UArr e -> SUArr e
{-# INLINE_U replicateSU #-}
replicateSU segd es = segd >: replicateEachU (sumU ns) ns es
  where
    ns = lengthsUSegd segd

-- |Yield a segmented array, where each element contains the same array value
--
replicateCU:: (UA e) => Int -> UArr e -> SUArr e
{-# INLINE_U replicateCU #-}
replicateCU n arr = segmentArrU (replicateU n rLen) $ repeatU n arr
  where
    rLen    = lengthU arr

repeatCU :: UA e => Int -> UArr Int -> USegd -> UArr e -> UArr e
{-# INLINE_U repeatCU #-}
repeatCU k ns segd xs
  = bpermuteU xs (repeatIndicesCU k ns segd)

repeatIndicesCU :: Int -> UArr Int -> USegd -> UArr Int
{-# INLINE_U repeatIndicesCU #-}
repeatIndicesCU k ns segd = enumFromStepLenEachU k
                          . mapU (\(n :*: i) -> i :*: 1 :*: n)
                          . replicateEachU n ns
                          $ fromUSegd segd 
  where
    n  = sumU ns

-- |Array indexing
--
(!:^) :: (UA e) => SUArr e -> UArr Int -> UArr e
{-# INLINE (!:^) #-}
(!:^) sArr inds = bpermuteU (concatSU sArr) newInds
  where
    xsLens  = lengthsSU sArr
    newInds = zipWithU (+) inds $ scanU (+) 0 xsLens    


-- |Segment an array according to the segmentation of the first argument
--
segmentU :: (UA e', UA e) => SUArr e' -> UArr e -> SUArr e
{-# INLINE_U segmentU #-}
segmentU template arr = segdSU template >: arr


-- |Segment an array according to the segmentation of the first argument
--
segmentArrU :: (UA e) => UArr Int -> UArr e -> SUArr e
{-# INLINE_U segmentArrU #-}
segmentArrU lengths arr = (lengthsToUSegd lengths) >: arr

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

-- |Associate each data element with its index
--
indexedSU :: UA e => SUArr e -> SUArr (Int :*: e)
{-# INLINE_U indexedSU #-}
indexedSU xss = segdSU xss >: zipU is xs
  where
    xs = concatSU xss

    is = enumFromToEachU (lengthU xs)
       . zipU (replicateU (lengthSU xss) 0)
       . mapU (subtract 1)
       $ lengthsSU xss

-- |Concatenation
-- --------------

appendSU :: UA a => USegd -> UArr a -> USegd -> UArr a -> UArr a
{-# INLINE_U appendSU #-}
appendSU xd xs yd ys = concatSU ((xd >: xs) ^+:+^ (yd >: ys))

infixr 5 ^+:+^

(^+:+^) :: UA a => SUArr a -> SUArr a -> SUArr a
{-# INLINE_U (^+:+^) #-}
xss ^+:+^ yss = toUSegd (zipU lens idxs)
                >: unstreamU (streamSU xss ^+++^ streamSU yss)
  where
    lens = zipWithU (+) (lengthsSU xss) (lengthsSU yss)
    idxs = zipWithU (+) (indicesSU xss) (indicesSU yss)

-- |Zipping
-- --------

fstSU :: (UA a, UA b) => SUArr (a :*: b) -> SUArr a
{-# INLINE_U fstSU #-}
fstSU sa = segdSU sa >: fstU (concatSU sa)

sndSU :: (UA a, UA b) => SUArr (a :*: b) -> SUArr b
{-# INLINE_U sndSU #-}
sndSU sa = segdSU sa >: sndU (concatSU sa)

zipSU :: (UA a, UA b) => SUArr a -> SUArr b -> SUArr (a :*: b)
{-# INLINE_U zipSU #-}
zipSU sa sb = segdSU sa >: zipU (concatSU sa) (concatSU sb)

-- |Enumeration functions
-- ----------------------

-- |Yield a segmented enumerated array
--
enumFromToSU :: (Enum e, UA e) => UArr e -> UArr e -> SUArr e
{-# INLINE_U enumFromToSU #-}
enumFromToSU starts = enumFromThenToSU starts (mapU succ starts)

-- |Yield a segmented enumerated array using a specific step
--
enumFromThenToSU :: (Enum e, UA e) 
		 => UArr e -> UArr e -> UArr e -> SUArr e
{-# INLINE_U enumFromThenToSU #-}
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
{-# INLINE_STREAM enumFromThenToEachS #-}
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

-- |Concatenate two arrays
--
-- FIXME: rename
--
(+:+^) :: UA e => SUArr e -> SUArr e -> SUArr e
{-# INLINE_U (+:+^) #-}
a1 +:+^ a2 = unstreamSU $ SStream (segs1 +++ segs2) (vals1 +++ vals2)
  where
    (SStream segs1 vals1) = streamSU a1  
    (SStream segs2 vals2) = streamSU a2

-- |Conversion
-- -----------

-- |Turn a nested list into a segmented parallel array
--
toSU :: UA e => [[e]] -> SUArr e
{-# INLINE_U toSU #-}
toSU ls = let lens = toU $ map length ls
	      a    = toU $ concat ls
          in
	  lengthsToUSegd lens >: a

-- |Turn a segmented array into a nested list
--
fromSU :: UA e => SUArr e -> [[e]]
{-# INLINE_U fromSU #-}
fromSU sa = let a       = concatSU sa
                lens    = fromU $ lengthsSU sa
                starts  = fromU $ indicesSU sa
            in
            [[a !: i | i <- [start .. start + len - 1]]
                            | (start, len) <- zip starts lens]

