-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.ListLike
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
--  Unlifted array versions of list-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.ListLike (
  module Data.Array.Parallel.Unlifted.Flat.ListLike,
  module Data.Array.Parallel.Unlifted.Segmented.ListLike
{-  -- * List-like combinators
  mapU,	(+:+), filterU, concatSU, {-concatMapU,-} nullU, lengthU, (!:), foldlU,
  foldlSU, foldl1U, scanlU, scanl1U, {-foldrU, foldr1U, scanrU, scanr1U,-}
  foldU, foldSU, fold1U, {-fold1SU,-} scanU, {-scanSU,-} scan1U, {-scan1SU,-}
  replicateU,
  takeU, dropU,	splitAtU, {-takeWhileU, dropWhileU, spanU, breakU,-}
--  lines, words, unlines, unwords,  -- is string processing really needed
  reverseU, andU, andSU, orU, orSU, anyU, allU, elemU, notElemU, {-lookupU,-}
  sumU, sumSU, productU, productSU, maximumU, maximumSU, minimumU, minimumSU, 
  zipU, zip3U, zipWithU, zipWith3U, unzipU, unzip3U, enumFromToU,
  enumFromToSU, enumFromThenToU, enumFromThenToSU, 
-}
) where

import Data.Array.Parallel.Unlifted.Flat.ListLike
import Data.Array.Parallel.Unlifted.Segmented.ListLike



