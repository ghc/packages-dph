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
  lengthU, nullU, emptyU, singletonU, consU, unitsU, replicateU, (!:), (+:+),
  indexedU,

  -- * Subarrays
  sliceU, extractU,
  takeU, dropU, splitAtU,
  {-takeWhileU, dropWhileU, spanU, breakU,-}

  -- * Permutations
  permuteU, bpermuteU, bpermuteDftU, reverseU, updateU,

  -- * Higher-order operations
  mapU, zipWithU, zipWith3U,
  filterU,
  foldlU, foldl1U, foldl1MaybeU,
  {-foldrU, foldr1U,-}
  foldU, fold1U, fold1MaybeU,
  scanlU, scanl1U,
  {-scanrU, scanr1U,-}
  scanU, scan1U,
  mapAccumLU,

  -- * Searching
  elemU, notElemU,
  {-lookupU, indexOfU,-}

  -- * Logical operations
  andU, orU, anyU, allU,

  -- * Arithmetic operations
  sumU, productU,
  maximumU, minimumU,
  maximumByU, minimumByU,
  maximumIndexU, minimumIndexU,
  maximumIndexByU, minimumIndexByU,

  -- * Arrays of pairs
  zipU, zip3U, unzipU, unzip3U, fstU, sndU,
  {-crossU,-}

  -- * Enumerations
  enumFromToU, enumFromThenToU, enumFromStepLenU,

  -- * Searching
  findU, findIndexU,

  -- * Conversions to\/from lists
  toU, fromU,

  -- * Random arrays
  randomU, randomRU,

  -- * I\/O
  UIO(..),

  -- * Operations on mutable arrays
  newU, lengthMU, newMU, readMU, writeMU, unsafeFreezeMU, unsafeFreezeAllMU,
  copyMU, permuteMU, updateMU, unstreamMU,

  -- FIXME
  lengthU'
) where

import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr, MUArr, UIO(..),
  newU, lengthMU, newMU, readMU, writeMU, copyMU,
  unsafeFreezeMU, unsafeFreezeAllMU)
import Data.Array.Parallel.Unlifted.Flat.Stream
import Data.Array.Parallel.Unlifted.Flat.Basics
import Data.Array.Parallel.Unlifted.Flat.Enum
import Data.Array.Parallel.Unlifted.Flat.Subarrays
import Data.Array.Parallel.Unlifted.Flat.Combinators
import Data.Array.Parallel.Unlifted.Flat.Search
import Data.Array.Parallel.Unlifted.Flat.Sums
import Data.Array.Parallel.Unlifted.Flat.Permute
import Data.Array.Parallel.Unlifted.Flat.Text ()
import Data.Array.Parallel.Unlifted.Flat.Random

