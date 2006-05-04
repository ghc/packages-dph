-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.SUArr
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

  -- * Segmentation
  concatSU, flattenSU, (>:), segmentU,

  -- * Basic operations
  replicateSU,

  -- * Permutations
  bpermuteSU,

  -- * Higher-order operations
  {-concatMapU,-}
  foldlSU, foldSU,
  {-fold1SU,-}
  {-scanSU, scan1SU,-}
  loopSU,

  -- * Logical operations
  andSU, orSU,

  -- * Arithmetic operations
  sumSU, productSU, maximumSU, minimumSU,

  -- * Enumerations
  enumFromToSU, enumFromThenToSU,

  -- * Conversions to/from lists
  toSU,

  -- * Operations on segment descriptors
  toUSegd, fromUSegd,
) where

import Data.Array.Parallel.Unlifted.Segmented.SUArr
import Data.Array.Parallel.Unlifted.Segmented.Loop
import Data.Array.Parallel.Unlifted.Segmented.Fusion    ({- rules only -})
import Data.Array.Parallel.Unlifted.Segmented.ListLike
import Data.Array.Parallel.Unlifted.Segmented.NeslLike

