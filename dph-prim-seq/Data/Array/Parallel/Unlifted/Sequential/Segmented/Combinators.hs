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

import Data.Array.Parallel.Stream (
  foldSS, fold1SS, combineSS, foldValuesR )
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd

import Debug.Trace

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> USegd -> Vector a -> Vector b
{-# INLINE_U foldlSU #-}
foldlSU f z segd xs = unstream
                    $ foldSS f z (stream (lengthsUSegd segd)) (stream xs)

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: Unbox a => (a -> a -> a) -> a -> USegd -> Vector a -> Vector a
foldSU = foldlSU

-- |Segmented array reduction from left to right with non-empty subarrays only
--
foldl1SU :: Unbox a => (a -> a -> a) -> USegd -> Vector a -> Vector a
{-# INLINE_U foldl1SU #-}
foldl1SU f segd xs = unstream
                   $ fold1SS f (stream (lengthsUSegd segd)) (stream xs)

-- |Segmented array reduction with non-empty subarrays and an associative
-- combination function
--
fold1SU :: Unbox a => (a -> a -> a) -> USegd -> Vector a -> Vector a
fold1SU = foldl1SU


-- |Merge two segmented arrays according to flag array
--
combineSU :: Unbox a => Vector Bool -> USegd -> Vector a -> USegd -> Vector a -> Vector a
{-# INLINE_U combineSU #-}
combineSU bs xd xs yd ys = unstream
                         $ combineSS (stream bs)
                                     (stream (lengthsUSegd xd)) (stream xs)
                                     (stream (lengthsUSegd yd)) (stream ys)

{-
combineCU::  Unbox e => Vector Bool -> SVector e -> SVector e -> SVector e
{-# INLINE combineCU #-}
combineCU  flags xssArr1 xssArr2 = trace "combineCU"
  newSegd >: flatData
  where
    newLengths = combineU  flags (lengthsSU xssArr1) (lengthsSU xssArr2)
    newSegd    = lengthsToUSegd newLengths
    repFlags   = replicateSU newSegd flags
    flatData   = combineU  (concatSU repFlags) (concatSU xssArr1)  (concatSU xssArr2)  

packCU:: (Unbox e) => Vector Bool -> SVector e -> SVector e
{-# INLINE packCU #-}
packCU flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = concatSU $ replicateSU (segdSU xssArr) flags
    flatData   = packU (concatSU xssArr) repFlags  
    newLengths = packU (lengthsSU xssArr) flags    
-}

-- | Regular arrar reduction 
--

foldlRU :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
{-# INLINE_U foldlRU #-}
foldlRU f z segSize = unstream . foldValuesR f z segSize . stream

