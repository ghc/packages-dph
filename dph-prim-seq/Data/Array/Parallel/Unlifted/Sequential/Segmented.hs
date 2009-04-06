-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented
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
-- Interface to operations on segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Sequential.Segmented (

  -- * Array types
  SUArr,

  -- * Streaming
  streamSU, unstreamSU,

  -- * Segmentation
  concatSU, (>:), segmentU, segmentArrU, segdSU,

  -- * Basic operations
  lengthSU, singletonSU, singletonsSU, replicateSU,
  sliceIndexSU, extractIndexSU, (+:+^),
  replicateCU, repeatCU, (!:^),
  indexedSU, (^+:+^), appendSU,

  -- * Basic operations lifted
  lengthsSU, indicesSU,

  -- * Subarrays
  sliceSU, extractSU, takeCU, dropCU,

  -- * Zipping
  fstSU, sndSU, zipSU,

  -- * Permutations
  bpermuteSU, bpermuteSU',

  -- * Higher-order operations
  mapSU, zipWithSU,
  {-concatMapU,-}
  foldlSU, foldlSU', foldSU, foldSU',
  fold1SU, fold1SU',
  {-scanSU, scan1SU,-}

  foldlRU,

  -- filter and combines
  filterSU, packCU, 

  combineSU, combineCU,
  -- * Logical operations
  andSU, orSU,

  -- * Arithmetic operations
  sumSU, productSU, maximumSU, minimumSU,
  sumRU,
  -- * Enumerations
  enumFromToSU, enumFromThenToSU,

  -- * Conversions to\/from lists
  toSU, fromSU,

  -- * Segment descriptors
  USegd, MUSegd,

  -- * Operations on segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd,
  lengthsToUSegd, toUSegd, fromUSegd
) where

import Data.Array.Parallel.Unlifted.Sequential.Segmented.SUArr
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Stream
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Subarrays
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Sums
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Permute
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Text ()

