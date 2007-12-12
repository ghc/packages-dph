-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented
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

module Data.Array.Parallel.Unlifted.Segmented (

  -- * Array types
  SUArr,

  -- * Streaming
  streamSU, unstreamSU,

  -- * Segmentation
  concatSU, flattenSU, (>:), segmentU, segmentArrU, segdSU,

  -- * Basic operations
  lengthSU, singletonSU, replicateSU, sliceIndexSU, extractIndexSU, (+:+^),
  indexedSU,

  -- * Basic operations lifted
  lengthsSU, indicesSU,

  -- * Subarrays
  sliceSU, extractSU,

  -- * Zipping
  fstSU, sndSU, zipSU,

  -- * Permutations
  bpermuteSU, bpermuteSU',

  -- * Higher-order operations
  mapSU, zipWithSU,
  {-concatMapU,-}
  foldlSU, foldSU,
  fold1SU,
  {-scanSU, scan1SU,-}

  -- filter and combines
  filterSU,

  combineSU,
  -- * Logical operations
  andSU, orSU,

  -- * Arithmetic operations
  sumSU, productSU, maximumSU, minimumSU,

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

import Data.Array.Parallel.Unlifted.Segmented.SUArr
import Data.Array.Parallel.Unlifted.Segmented.Stream
import Data.Array.Parallel.Unlifted.Segmented.Basics
import Data.Array.Parallel.Unlifted.Segmented.Subarrays
import Data.Array.Parallel.Unlifted.Segmented.Combinators
import Data.Array.Parallel.Unlifted.Segmented.Sums
import Data.Array.Parallel.Unlifted.Segmented.Permute
import Data.Array.Parallel.Unlifted.Segmented.Text ()

