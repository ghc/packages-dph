-- | Distributed types and operations.
--
--   * This is an internal API and shouldn't need to be used directly.
--     Client programs should use "Data.Array.Parallel.Unlifted"
--
module Data.Array.Parallel.Unlifted.Distributed (
  -- * Gang operations
  Gang, forkGang, gangSize,

  -- * Gang hacks
  theGang,

  -- * Distributed types and classes
  DT(..),

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
  lengthD, splitLenD, splitLenIdxD,
  splitD, splitAsD, joinLengthD, joinD, splitJoinD, joinDM,
  glueSegdD, carryD,
  Distribution, balanced, unbalanced,

  -- * Permutations
  permuteD, bpermuteD, atomicUpdateD,

  -- * Debugging
  fromD, toD, debugD
) where

import Data.Array.Parallel.Unlifted.Distributed.TheGang
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Scalars
import Data.Array.Parallel.Unlifted.Distributed.Arrays
import Data.Array.Parallel.Unlifted.Distributed.USegd
import Data.Array.Parallel.Unlifted.Distributed.Basics
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.Gang (Gang, forkGang, gangSize)

