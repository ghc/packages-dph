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

  enumFromToUP, enumFromThenToUP, enumFromStepLenUP,

  mapUP, filterUP, zipWithUP, foldUP, scanUP,

  andUP, sumUP,

  zipWithSUP, foldSUP, sumSUP, bpermuteSUP', enumFromThenToSUP,

  indexedUP, replicateUP, replicateEachUnbalancedUP,

  dropUP
) where

import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Sums
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Segmented
import Data.Array.Parallel.Unlifted.Parallel.Subarrays


