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
  foldlSU, foldSU, foldl1SU, fold1SU, {-scanSU,-} {-scan1SU,-}
  foldlRU,
  combineSU
) where

import Data.Array.Parallel.Base (
  sndS)
import Data.Array.Parallel.Stream (
  foldSS, fold1SS, combineSS, foldValuesR )
import Data.Array.Parallel.Unlifted.Sequential.Flat (
  UA, UArr, mapU, zipWithU,
  unstreamU, streamU)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd

import Debug.Trace

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> USegd -> UArr a -> UArr b
{-# INLINE_U foldlSU #-}
foldlSU f z segd !xs = unstreamU
                     $ foldSS f z (streamU (lengthsUSegd segd)) (streamU xs)

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> USegd -> UArr a -> UArr a
foldSU = foldlSU

-- |Segmented array reduction from left to right with non-empty subarrays only
--
foldl1SU :: UA a => (a -> a -> a) -> USegd -> UArr a -> UArr a
{-# INLINE_U foldl1SU #-}
foldl1SU f segd xs = unstreamU
                   $ fold1SS f (streamU (lengthsUSegd segd)) (streamU xs)

-- |Segmented array reduction with non-empty subarrays and an associative
-- combination function
--
fold1SU :: UA a => (a -> a -> a) -> USegd -> UArr a -> UArr a
fold1SU = foldl1SU


-- |Merge two segmented arrays according to flag array
--
combineSU :: UA a => UArr Bool -> USegd -> UArr a -> USegd -> UArr a -> UArr a
{-# INLINE_U combineSU #-}
combineSU bs xd xs yd ys = unstreamU
                         $ combineSS (streamU bs)
                                     (streamU (lengthsUSegd xd)) (streamU xs)
                                     (streamU (lengthsUSegd yd)) (streamU ys)

{-
combineCU::  UA e => UArr Bool -> SUArr e -> SUArr e -> SUArr e
{-# INLINE combineCU #-}
combineCU  flags xssArr1 xssArr2 = trace "combineCU"
  newSegd >: flatData
  where
    newLengths = combineU  flags (lengthsSU xssArr1) (lengthsSU xssArr2)
    newSegd    = lengthsToUSegd newLengths
    repFlags   = replicateSU newSegd flags
    flatData   = combineU  (concatSU repFlags) (concatSU xssArr1)  (concatSU xssArr2)  

packCU:: (UA e) => UArr Bool -> SUArr e -> SUArr e
{-# INLINE packCU #-}
packCU flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = concatSU $ replicateSU (segdSU xssArr) flags
    flatData   = packU (concatSU xssArr) repFlags  
    newLengths = packU (lengthsSU xssArr) flags    
-}

-- | Regular arrar reduction 
--

foldlRU :: (UA a, UA b) => (b -> a -> b) -> b -> Int -> Int -> UArr a -> UArr b
{-# INLINE_U foldlRU #-}
foldlRU f z noOfSegs segSize = unstreamU . foldValuesR f z noOfSegs segSize . streamU

