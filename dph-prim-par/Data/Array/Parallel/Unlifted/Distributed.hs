-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Distributed types and operations.
--

module Data.Array.Parallel.Unlifted.Distributed (
  -- * Gang operations
  Gang, forkGang, gangSize, sequentialGang, seqGang,

  -- * Gang hacks
  theGang,

  -- * Distributed types and classes
  DT, Dist,

  -- * Higher-order combinators
  mapD, zipWithD, foldD, scanD,

  -- * Equality
  eqD, neqD,

  -- * Distributed scalars
  scalarD,
  andD, orD, sumD,

  -- * Distributed pairs
  zipD, unzipD, fstD, sndD,

  -- * Distributed arrays
  lengthD, splitLenD, splitLengthD,
  splitD, splitAsD, joinLengthD, joinD, splitJoinD,
  splitSegdD, splitSD,
  lengthUSegdD, lengthsUSegdD, indicesUSegdD, elementsUSegdD,
  Distribution, balanced, unbalanced,

  -- * Permutations
  permuteD, bpermuteD, atomicUpdateD,

  -- * Debugging
  fromD, toD
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, forkGang, gangSize, sequentialGang, seqGang)
import Data.Array.Parallel.Unlifted.Distributed.TheGang
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Scalars
import Data.Array.Parallel.Unlifted.Distributed.Arrays
import Data.Array.Parallel.Unlifted.Distributed.Basics

