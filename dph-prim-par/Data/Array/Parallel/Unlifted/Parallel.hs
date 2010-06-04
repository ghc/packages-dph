-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Parallel operations on unlifted arrays
--

module Data.Array.Parallel.Unlifted.Parallel (
  bpermuteUP, updateUP,

  enumFromToUP, enumFromThenToUP, enumFromStepLenUP, enumFromStepLenEachUP, enumFromToEachUP,

  mapUP, filterUP, packUP, combineUP, combine2ByTagUP,
  zipWithUP, foldUP, scanUP,

  andUP, sumUP,
  
  replicateSUP, replicateRSUP, appendSUP, indicesSUP,
  foldSUP, foldRUP, fold1SUP, sumSUP, sumRUP,

  indexedUP, replicateUP, repeatUP, interleaveUP,

  dropUP
) where

import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Sums
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Segmented
import Data.Array.Parallel.Unlifted.Parallel.Subarrays


