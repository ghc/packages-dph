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
  DT, MDT, Dist,

  -- * Higher-order combinators
  mapD, zipWithD, foldD, scanD,

  -- * Distributed scalars
  splitScalarD,

  -- * Distributed pairs
  zipD, unzipD, fstD, sndD,

  -- * Distributed arrays
  splitLengthD, splitD, joinD, lengthsD, lengthD,

  -- * Permutations
  permuteD, bpermuteD,

  -- * Debugging
  fromD, toD
) where

import Data.Array.Parallel.Distributed.Gang (
  Gang, forkGang, gangSize, sequentialGang, seqGang)
import Data.Array.Parallel.Distributed.Types (
  DT, MDT, Dist)
import Data.Array.Parallel.Distributed.Operations (
  zipD, unzipD, fstD, sndD,
  mapD, zipWithD, foldD, scanD,
  splitScalarD, splitLengthD, splitD, joinD,
  lengthsD, lengthD,
  permuteD, bpermuteD,
  fromD, toD)

