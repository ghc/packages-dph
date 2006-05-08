-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Distributed types and operations.
--

module Data.Array.Parallel.Distributed (
  -- * Gang operations
  Gang, forkGang, gangSize, sequentialGang, seqGang,

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
  lengthD, splitLengthD, splitD, joinLengthD, joinD,

  -- * Permutations
  permuteD, bpermuteD,

  -- * Debugging
  fromD, toD
) where

import Data.Array.Parallel.Distributed.Gang (
  Gang, forkGang, gangSize, sequentialGang, seqGang)
import Data.Array.Parallel.Distributed.Types (
  DT, Dist)
import Data.Array.Parallel.Distributed.Combinators
import Data.Array.Parallel.Distributed.Scalars
import Data.Array.Parallel.Distributed.Arrays
import Data.Array.Parallel.Distributed.Basics

