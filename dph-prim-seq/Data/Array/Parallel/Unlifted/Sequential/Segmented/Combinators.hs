-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators
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

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators (
  mapSU, zipWithSU,
  foldlSU, foldlSU', foldSU, foldSU', foldl1SU, fold1SU, {-scanSU,-} {-scan1SU,-}
  combineSU, combineCU,
  filterSU, packCU
) where

import Data.Array.Parallel.Base (
  sndS)
import Data.Array.Parallel.Stream
import Data.Array.Parallel.Unlifted.Sequential.Flat (
  UA, UArr, mapU, zipWithU,
  unstreamU, streamU)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.SUArr (
  SUArr, USegd, segdSU, concatSU, (>:), lengthsSU, lengthsUSegd, lengthsToUSegd)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics (
  concatSU, segmentArrU,segmentU, replicateSU)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Stream (
  streamSU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators (
  filterU, combineU, packU)


import Debug.Trace

mapSU :: (UA a, UA b) => (a -> b) -> SUArr a -> SUArr b
{-# INLINE_U mapSU #-}
mapSU f sa = segdSU sa >: mapU f (concatSU sa)

zipWithSU :: (UA a, UA b, UA c)
          => (a -> b -> c) -> SUArr a -> SUArr b -> SUArr c
{-# INLINE_U zipWithSU #-}
zipWithSU f sa sb = segdSU sa >: zipWithU f (concatSU sa) (concatSU sb)

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE_U foldlSU #-}
foldlSU f z = unstreamU . foldValuesSS f z . streamSU

foldlSU' :: (UA a, UA b) => (b -> a -> b) -> b -> USegd -> UArr a -> UArr b
{-# INLINE_U foldlSU' #-}
foldlSU' f z segd xs = unstreamU
                      (foldValuesSS' f z (streamU (lengthsUSegd segd))
                                         (streamU xs))

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

foldSU' :: UA a => (a -> a -> a) -> a -> USegd -> UArr a -> UArr a
foldSU' = foldlSU'

-- |Segmented array reduction from left to right with non-empty subarrays only
--
foldl1SU :: UA a => (a -> a -> a) -> SUArr a -> UArr a
{-# INLINE_U foldl1SU #-}
foldl1SU f = unstreamU . fold1ValuesSS f . streamSU

foldl1SU' :: UA a => (a -> a -> a) -> USegd -> UArr a -> UArr a
{-# INLINE_U foldl1SU' #-}
foldl1SU' f segd xs = unstreamU
                     (fold1ValuesSS' f (streamU (lengthsUSegd segd))
                                       (streamU xs))

-- |Segmented array reduction with non-empty subarrays and an associative
-- combination function
--
fold1SU :: UA a => (a -> a -> a) -> SUArr a -> UArr a
fold1SU = foldl1SU

fold1SU' :: UA a => (a -> a -> a) -> USegd -> UArr a -> UArr a
fold1SU' = foldl1SU'


-- |Merge two segmented arrays according to flag array
--
combineSU:: UA a => UArr Bool -> SUArr a -> SUArr a -> UArr a
{-# INLINE_U combineSU #-}
combineSU fs xs1 xs2 =
  unstreamU $ combineSS (streamU fs) (streamSU xs1) (streamSU xs2)

combineCU::  UA e => UArr Bool -> SUArr e -> SUArr e -> SUArr e
{-# INLINE combineCU #-}
combineCU  flags xssArr1 xssArr2 = trace "combineCU"
  newSegd >: flatData
  where
    newLengths = combineU  flags (lengthsSU xssArr1) (lengthsSU xssArr2)
    newSegd    = lengthsToUSegd newLengths
    repFlags   = replicateSU newSegd flags
    flatData   = combineU  (concatSU repFlags) (concatSU xssArr1)  (concatSU xssArr2)  

-- |Filter segmented array
--
filterSU:: (UA e) => (e -> Bool) -> SUArr e -> SUArr e
{-# INLINE_U filterSU #-}
filterSU p xssArr = segmentArrU newLengths flatData
  where
    flatData   = filterU p $ concatSU xssArr
    segdFlags  = segmentU xssArr $ mapU (\x -> if p x then 1 else 0) $ concatSU xssArr
    newLengths = foldSU (+) 0 segdFlags 


packCU:: (UA e) => UArr Bool -> SUArr e -> SUArr e
{-# INLINE packCU #-}
packCU flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = concatSU $ replicateSU (segdSU xssArr) flags
    flatData   = packU (concatSU xssArr) repFlags  
    newLengths = packU (lengthsSU xssArr) flags    


-- | Regular arrar reduction 
--

foldlRU :: (UA a, UA b) => (b -> a -> b) -> b -> Int -> Int -> UArr a -> UArr b
{-# INLINE_U foldlRU #-}
foldlRU f z noOfSegs segSize = unstreamU . foldValuesR f z noOfSegs segSize . streamU
