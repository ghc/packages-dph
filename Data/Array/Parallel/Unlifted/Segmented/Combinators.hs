-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Combinators
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
--  Standard combinators for segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Combinators (
  mapSU, zipWithSU,
  foldlSU, foldSU, foldl1SU, fold1SU, {-scanSU,-} {-scan1SU,-}
  combineSU, filterSU
) where

import Data.Array.Parallel.Base (
  sndS)
import Data.Array.Parallel.Stream (
  Stream, SStream, mapS, foldValuesSS, fold1ValuesSS, combineSS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr, mapU, zipWithU,
  unstreamU, streamU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, segdSU, flattenSU, (>:))
import Data.Array.Parallel.Unlifted.Segmented.Basics (
  concatSU, segmentArrU,segmentU)
import Data.Array.Parallel.Unlifted.Segmented.Stream (
  streamSU)
import Data.Array.Parallel.Unlifted.Flat.Combinators (
  filterU)


import Debug.Trace

mapSU :: (UA a, UA b) => (a -> b) -> SUArr a -> SUArr b
{-# INLINE mapSU #-}
mapSU f sa = segdSU sa >: mapU f (concatSU sa)

zipWithSU :: (UA a, UA b, UA c)
          => (a -> b -> c) -> SUArr a -> SUArr b -> SUArr c
{-# INLINE zipWithSU #-}
zipWithSU f sa sb = segdSU sa >: zipWithU f (concatSU sa) (concatSU sb)

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSU #-}
foldlSU f z = unstreamU . foldValuesSS f z . streamSU

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

-- |Segmented array reduction from left to right with non-empty subarrays only
--
foldl1SU :: UA a => (a -> a -> a) -> SUArr a -> UArr a
{-# INLINE foldl1SU #-}
foldl1SU f = unstreamU . fold1ValuesSS f . streamSU

-- |Segmented array reduction with non-empty subarrays and an associative
-- combination function
--
fold1SU :: UA a => (a -> a -> a) -> SUArr a -> UArr a
fold1SU = foldl1SU


-- |Merge two segmented arrays according to flag array
--
combineSU:: UA a => UArr Bool -> SUArr a -> SUArr a -> UArr a
{-# INLINE combineSU #-}
combineSU fs xs1 xs2 =
  unstreamU $ combineSS (streamU fs) (streamSU xs1) (streamSU xs2)

-- |Filter segmented array
--
filterSU:: (UA e) => (e -> Bool) -> SUArr e -> SUArr e
{-# INLINE filterSU #-}
filterSU p xssArr = segmentArrU newLengths flatData
  where
    flatData   = filterU p $ flattenSU xssArr
    segdFlags  = segmentU xssArr $ mapU (\x -> if p x then 1 else 0) $ flattenSU xssArr
    newLengths = foldSU (+) 0 segdFlags 



