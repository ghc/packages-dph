-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- External interface to unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted (

  -- * Array classes
  UA,

  -- * Array types
  UArr, USegd, SUArr,

  -- * Strict pairs and sums
  (:*:)(..), (:+:)(..),

  -- * List-like combinators
  mapU,	(+:+), filterU, concatSU, {-concatMapU,-} nullU, lengthU, (!:), foldlU,
  foldlSU, {-foldl1U,-} scanlU, {-scanl1U, foldrU, foldr1U, scanrU, scanr1U,-}
  foldU, foldSU, {-fold1U, fold1SU,-} scanU, {-scanSU, scan1U, scan1SU,-}
  takeU, dropU,	splitAtU, {-takeWhileU, dropWhileU, spanU, breakU,-}
--  lines, words, unlines, unwords,  -- is string processing really needed
  reverseU, andU, andSU, orU, orSU, anyU, allU, elemU, notElemU, {-lookupU,-}
  sumU, sumSU, productU, productSU, maximumU, maximumSU, minimumU, minimumSU,
  zipU, zip3U, zipWithU, zipWith3U, unzipU, unzip3U, enumFromToU,
  enumFromToSU, enumFromThenToU, enumFromThenToSU, 

  -- * Nesl-like combinators
  --
  flattenSU, (>:), toUSegd, fromUSegd, segmentU, toU, toSU, fromU, emptyU,
  extractU, permuteU, bpermuteU, bpermuteSU, bpermuteDftU, {-crossU, indexOfU,
  -}

  -- * Loop/replicate combinators
  replicateU, loopU, replicateSU, loopSU,

  -- * Projection combinators for loops
  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc,

  -- * Special forms of loop mutators
  noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL,

  -- * Library id
  idstr, name, versnum, date, version, copyright, disclaimer

) where

import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Monadic.UArr (
  UA, UArr)
import Data.Array.Parallel.Monadic.SUArr (
  USegd, SUArr, fromUSegd, toUSegd)
import Data.Array.Parallel.Declarative.Loop (
  replicateU, loopU, replicateSU, loopSU, loopArr, loopArrS, loopAcc,
  loopAccS, loopSndAcc)
import Data.Array.Parallel.Declarative.Fusion (
  noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL)
import Data.Array.Parallel.Unlifted.ListLike
import Data.Array.Parallel.Unlifted.NeslLike


-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: FIXME: Have the build-system produce an id$"
name       = "Unlifted Array Library"
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
instance (Show e, UA e) => Show (UArr e) where
  showsPrec _  = (showString "toU " .) . showList . fromU

-- |
instance Show USegd where
  showsPrec _ segd =   showString "toUSegd (toU "
		     . showList (fromU (fromUSegd segd))
		     . showChar ')'

-- |
instance (Eq e, UA e) => Eq (UArr e) where
  a1 == a2 = foldlU cmp True (zipU a1 a2)
	     where
	       cmp r (e1 :*: e2) = e1 == e2 && r
