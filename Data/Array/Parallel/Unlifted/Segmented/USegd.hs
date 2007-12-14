-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.USegd
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
-- Segment descriptors.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Segmented.USegd (

  -- * Types
  USegd, MUSegd,

  -- * Operations on segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd, fromUSegd,
  singletonUSegd, lengthsToUSegd, toUSegd,
  sliceUSegd, extractUSegd,
  newMUSegd, unsafeFreezeMUSegd,
) where

import Data.Array.Parallel.Base (
  (:*:)(..), ST)
import Data.Array.Parallel.Unlifted.Flat (
  UArr, MUArr,
  lengthU, singletonU, (!:), sliceU, extractU, mapU,
  scanlU, mapAccumLU,
  fstU, sndU, zipU,
  streamU, unstreamU,
  newMU, unsafeFreezeMU)

import Control.Monad (
  liftM)


-- | Segment descriptors represent the structure of nested arrays. For each
-- segment, it stores the length and the starting index in the flat data
-- array.
--
newtype USegd    = USegd  { unUSegd  :: UArr  (Int :*: Int)   }
newtype MUSegd s = MUSegd { unMUSegd :: MUArr (Int :*: Int) s }

-- |Operations on segment descriptors
-- ----------------------------------

-- |Allocate a mutable segment descriptor for the given number of segments
--
newMUSegd :: Int -> ST s (MUSegd s)
{-# INLINE newMUSegd #-}
newMUSegd = liftM MUSegd . newMU

-- |Convert a mutable segment descriptor to an immutable one
--
unsafeFreezeMUSegd :: MUSegd s -> Int -> ST s USegd
{-# INLINE unsafeFreezeMUSegd #-}
unsafeFreezeMUSegd sd n = liftM USegd (unsafeFreezeMU (unMUSegd sd) n)

-- |Yield the overall number of segments
--
lengthUSegd :: USegd -> Int
{-# INLINE lengthUSegd #-}
lengthUSegd = lengthU . unUSegd

-- |Yield the segment lengths of a segment descriptor
--
lengthsUSegd :: USegd -> UArr Int
{-# INLINE lengthsUSegd #-}
lengthsUSegd = fstU . unUSegd

-- |Yield the segment indices of a segment descriptor
--
indicesUSegd :: USegd -> UArr Int
{-# INLINE indicesUSegd #-}
indicesUSegd = sndU . unUSegd

-- |Convert a segment descriptor to an array of length\/index pairs.
--
fromUSegd :: USegd -> UArr (Int :*: Int)
{-# INLINE fromUSegd #-}
fromUSegd = unUSegd

-- |Convert an array of length\/index pairs to a segment descriptor.
--
toUSegd :: UArr (Int :*: Int) -> USegd
{-# INLINE toUSegd #-}
toUSegd = USegd

-- |Yield a singleton segment descriptor
--
singletonUSegd :: Int -> USegd
{-# INLINE singletonUSegd #-}
singletonUSegd n = toUSegd $ singletonU (n :*: 0)

-- |Convert a length array into a segment descriptor.
--
lengthsToUSegd :: UArr Int -> USegd
{-# INLINE lengthsToUSegd #-}
lengthsToUSegd = USegd . segdFromLengthsU

-- |Convert a length array to an array of length\/index pairs.
--
segdFromLengthsU :: UArr Int -> UArr (Int :*: Int)
{-# INLINE_STREAM segdFromLengthsU #-}
segdFromLengthsU lens = zipU lens (scanlU (+) 0 lens)

-- |Convert a length array to an array of length\/index pairs - fusible
-- version.
--
segdFromLengthsU' :: UArr Int -> UArr (Int :*: Int)
{-# INLINE_U segdFromLengthsU' #-}
segdFromLengthsU' = mapAccumLU (\i n -> (i + n) :*: (n :*: i)) 0

{-# RULES

"segdFromLengthsU/unstreamU" forall s.
  segdFromLengthsU (unstreamU s) = segdFromLengthsU' (unstreamU s)
"streamU/fromLengthsU" forall a.
  streamU (segdFromLengthsU a) = streamU (segdFromLengthsU' a)
 #-}


-- |Extract a slice of a segment descriptor, avoiding copying where possible.
--
-- NOTE: In the new segment descriptor, the starting index of the first
--       segment will be 0.
--
sliceUSegd :: USegd -> Int -> Int -> USegd
sliceUSegd segd i n = USegd (zipU lens idxs')
  where
    lens  = sliceU (lengthsUSegd segd) i n
    idxs  = sliceU (indicesUSegd segd) i n
    idxs' = mapU (subtract k) idxs
    k     = idxs !: 0

-- |Extract a slice of a segment descriptor, copying everything.
--
-- NOTE: In the new segment descriptor, the starting index of the first
--       segment will be 0.
--
extractUSegd :: USegd -> Int -> Int -> USegd
extractUSegd segd i n = USegd (zipU lens idxs')
  where
    lens  = extractU (lengthsUSegd segd) i n
    idxs  = sliceU   (indicesUSegd segd) i n
    idxs' = mapU (subtract k) idxs
    k     = idxs !: 0

