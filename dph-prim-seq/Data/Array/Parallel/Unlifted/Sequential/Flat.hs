-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.SUArr
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
-- Interface to operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Sequential.Flat (

  -- * Array classes
  UA,

  -- * Array types
  UArr, MUArr,

  -- * Streaming
  streamU, unstreamU,

  -- * Basic operations
  lengthU, nullU, emptyU, singletonU, consU, unitsU,
  replicateU,
  -- replicateEachU,
  (!:), (+:+),
  indexedU, repeatU, repeatUS,

  -- * Subarrays
  sliceU, extractU,
  tailU,
  takeU, dropU, splitAtU,
  {-takeWhileU, dropWhileU, spanU, breakU,-}

  -- * Permutations
  permuteU, bpermuteU, mbpermuteU, bpermuteDftU, reverseU, updateU,


  -- * Higher-order operations
  mapU, zipWithU, zipWith3U,
  filterU, packU, 
  combineU, 
  foldlU, foldl1U, foldl1MaybeU,
  {-foldrU, foldr1U,-}
  foldU, fold1U, fold1MaybeU,
  scanlU, scanl1U,
  {-scanrU, scanr1U,-}
  scanU, scan1U,
  scanResU,
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
  enumFromToU, enumFromThenToU, enumFromStepLenU, enumFromToEachU, enumFromStepLenEachU,

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
  copyMU, permuteMU, atomicUpdateMU, unstreamMU,
  hasAtomicWriteMU, atomicWriteMU,

  -- FIXME
  lengthU'
) where

import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr, MUArr, UIO(..),
  newU, lengthMU, newMU, readMU, writeMU, copyMU,
  unsafeFreezeMU, unsafeFreezeAllMU,
  hasAtomicWriteMU, atomicWriteMU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream
import Data.Array.Parallel.Unlifted.Sequential.Flat.Basics
import Data.Array.Parallel.Unlifted.Sequential.Flat.Enum
import Data.Array.Parallel.Unlifted.Sequential.Flat.Subarrays
import Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Flat.Search
import Data.Array.Parallel.Unlifted.Sequential.Flat.Sums
import Data.Array.Parallel.Unlifted.Sequential.Flat.Permute
import Data.Array.Parallel.Unlifted.Sequential.Flat.Text ()
import Data.Array.Parallel.Unlifted.Sequential.Flat.Random

