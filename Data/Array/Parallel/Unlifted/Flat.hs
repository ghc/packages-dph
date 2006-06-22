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
-- Interface to operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat (

  -- * Array classes
  UA,

  -- * Array types
  UArr, MUArr,

  -- * Streaming
  streamU, unstreamU,

  -- * Basic operations
  lengthU, nullU, emptyU, unitsU, replicateU, (!:), (+:+),

  -- * Subarrays
  sliceU, extractU,
  takeU, dropU, splitAtU,
  {-takeWhileU, dropWhileU, spanU, breakU,-}

  -- * Permutations
  permuteU, bpermuteU, bpermuteDftU, reverseU,

  -- * Higher-order operations
  mapU, zipWithU, zipWith3U,
  filterU,
  foldlU, foldl1U,
  {-foldrU, foldr1U,-}
  foldU, fold1U,
  scanlU, scanl1U,
  {-scanrU, scanr1U,-}
  scanU, scan1U,

  -- * Searching
  elemU, notElemU,
  {-lookupU, indexOfU,-}

  -- * Logical operations
  andU, orU, anyU, allU,

  -- * Arithmetic operations
  sumU, productU, maximumU, minimumU,

  -- * Arrays of pairs
  zipU, zip3U, unzipU, unzip3U,
  {-crossU,-}

  -- * Enumerations
  enumFromToU, enumFromThenToU,

  -- * Conversions to/from lists
  toU, fromU,

  -- * Operations on mutable arrays
  newU, newMU, readMU, writeMU, unsafeFreezeMU, copyMU, permuteMU,

  -- FIXME
  lengthU'
) where

import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr, MUArr,
  newU, newMU, readMU, writeMU, copyMU, unsafeFreezeMU)
import Data.Array.Parallel.Unlifted.Flat.Stream
import Data.Array.Parallel.Unlifted.Flat.Basics
import Data.Array.Parallel.Unlifted.Flat.Subarrays
import Data.Array.Parallel.Unlifted.Flat.Combinators
import Data.Array.Parallel.Unlifted.Flat.Sums
import Data.Array.Parallel.Unlifted.Flat.Permute

