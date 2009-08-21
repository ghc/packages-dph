-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) [2006..2007] Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- External interface to unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
-- 

module Data.Array.Parallel.Unlifted.Sequential (

  -- * Array classes
  UA,

  -- * Array types
  UArr, USegd,

  -- * Basic operations
  lengthU, nullU, emptyU, singletonU, consU, unitsU,
  replicateU, (!:), (+:+),
  indexedU, repeatU,

  -- * Subarrays
  sliceU, extractU,
  tailU,
  takeU, dropU, splitAtU,
  {-takeWhileU, dropWhileU, spanU, breakU,-}

  -- * Permutations
  permuteU, mbpermuteU, bpermuteU, bpermuteDftU, reverseU, updateU,


  -- * Higher-order operations
  mapU, zipWithU, zipWith3U,
  filterU, packU, 
  foldlU, foldl1U, foldl1MaybeU,
  {-foldrU, foldr1U,-}
  foldU, fold1U, fold1MaybeU,
  scanlU, scanl1U,
  {-scanrU, scanr1U,-}
  scanU, scan1U,
  scanResU,
  mapAccumLU,

  -- Segmented filter and combines
  combineU, combineSU,

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

  -- * Unlifted.Sequential arrays
  randomU, randomRU,

  -- * I\/O
  UIO(..),

  -- * Basic operations (segmented)
  {-repeatCU,-} replicateSU, replicateRSU, appendSU,

  -- * Higher-order operations (segmented)
  foldlSU, foldSU, fold1SU,
  {-scanSU, scan1SU,-}

  -- * Higher-order operations (regular)
  foldlRU,

  -- * Logical operations (segmented)
  andSU, orSU,

  -- * Arithmetic operations (segmented)
  sumSU, productSU, maximumSU, minimumSU,
 
  -- * Arithmetic operations (regular segmented)
  sumRU, 

  -- * Segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd, elementsUSegd, lengthsToUSegd, mkUSegd,

  -- * Mutable arrays
  MUArr, newU, newMU, copyMU, permuteMU,
  hasAtomicWriteMU, atomicUpdateMU, unsafeFreezeAllMU,

  -- * Library id
  idstr, name, versnum, date, version, copyright, disclaimer,

  -- FIXME
  lengthU'

) where

import Data.Array.Parallel.Base ((:*:)(..))
import Data.Array.Parallel.Unlifted.Sequential.Flat
import Data.Array.Parallel.Unlifted.Sequential.Segmented

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: FIXME: Have the build-system produce an id$"
name       = "Unlifted.Sequential Array Library"
versnum    = "0.6.0"
date	   = "28 Apr 2006"
version    = name ++ ", version " ++ versnum ++ ", " ++ date
copyright  = "Copyright (c) [2001..2006] \
	     \M M T Chakravarty, G Keller & R Leshchinskiy"
disclaimer = "This software is distributed under the terms \
	     \of the BSD3 license.  NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."


-- |Parallel array instances of standard classes
-- ---------------------------------------------

-- |
instance (Eq e, UA e) => Eq (UArr e) where
  a1 == a2 = lengthU a1 == lengthU a2 && foldlU cmp True (zipU a1 a2)
	     where
	       cmp r (e1 :*: e2) = e1 == e2 && r

