-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented
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
-- Interface to operations on segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented (

  -- * Array types
  USegd, SUArr,

  -- * Streaming
  streamSU, unstreamSU,

  -- * Segmentation
  concatSU, flattenSU, (>:), segmentU,

  -- * Basic operations
  lengthSU, replicateSU, sliceIndexSU, extractIndexSU,

  -- * Subarrays
  sliceSU, extractSU,

  -- * Permutations
  bpermuteSU,

  -- * Higher-order operations
  {-concatMapU,-}
  foldlSU, foldSU,
  {-fold1SU,-}
  {-scanSU, scan1SU,-}

  -- * Logical operations
  andSU, orSU,

  -- * Arithmetic operations
  sumSU, productSU, maximumSU, minimumSU,

  -- * Enumerations
  enumFromToSU, enumFromThenToSU,

  -- * Conversions to/from lists
  toSU, fromSU,
) where

import Data.Array.Parallel.Unlifted.Segmented.SUArr
import Data.Array.Parallel.Unlifted.Segmented.Stream
import Data.Array.Parallel.Unlifted.Segmented.Basics
import Data.Array.Parallel.Unlifted.Segmented.Subarrays
import Data.Array.Parallel.Unlifted.Segmented.Combinators
import Data.Array.Parallel.Unlifted.Segmented.Sums
import Data.Array.Parallel.Unlifted.Segmented.Permute
import Data.Array.Parallel.Unlifted.Segmented.Text ()

