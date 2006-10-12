-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.USegd
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
-- Segment descriptors.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.USegd (

  -- * Types
  USegd, MUSegd,

  -- * Operations on segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd, toUSegd,
  sliceUSegd, extractUSegd,
  newMUSegd, unsafeFreezeMUSegd,
) where

import Data.Array.Parallel.Base (
  (:*:)(..), ST)
import Data.Array.Parallel.Unlifted.Flat (
  UArr, MUArr,
  lengthU, (!:), sliceU, extractU, mapU, scanlU,
  fstU, sndU, zipU,
  streamU, unstreamU,
  newMU, unsafeFreezeMU)


-- | Segment descriptors represent the structure of nested arrays. For each
-- segment, it stores the length and the starting index in the flat data
-- array.
--
-- NOTE: We don't use a newtype to allow regular array fusion to work.
--
type USegd  = UArr  (Int :*: Int)
type MUSegd = MUArr (Int :*: Int)

-- |Operations on segment descriptors
-- ----------------------------------

-- |Allocate a mutable segment descriptor for the given number of segments
--
newMUSegd :: Int -> ST s (MUSegd s)
{-# INLINE newMUSegd #-}
newMUSegd = newMU

-- |Convert a mutable segment descriptor to an immutable one
--
unsafeFreezeMUSegd :: MUSegd s -> Int -> ST s USegd
{-# INLINE unsafeFreezeMUSegd #-}
unsafeFreezeMUSegd = unsafeFreezeMU

-- |Yield the overall number of segments
--
lengthUSegd :: USegd -> Int
{-# INLINE lengthUSegd #-}
lengthUSegd = lengthU

-- |Yield the segment lengths of a segment descriptor
--
lengthsUSegd :: USegd -> UArr Int
{-# INLINE lengthsUSegd #-}
lengthsUSegd = fstU

-- |Yield the segment indices of a segment descriptor
--
indicesUSegd :: USegd -> UArr Int
{-# INLINE indicesUSegd #-}
indicesUSegd = sndU

-- |Convert a length array into a segment descriptor.
--
toUSegd :: UArr Int -> USegd
{-# INLINE [1] toUSegd #-}
toUSegd lens = zipU lens (scanlU (+) 0 lens)

-- |Convert a length array to a segment descriptor - fusible version.
--
toUSegd' :: UArr Int -> USegd
{-# INLINE toUSegd' #-}
toUSegd' = scanlU (\(_ :*: i) n -> n :*: (i+n)) (0 :*: 0)

{-# RULES

"toUSegd/unstreamU" forall s.
  toUSegd (unstreamU s) = toUSegd' (unstreamU s)
"streamU/toUSegd" forall a.
  streamU (toUSegd a) = streamU (toUSegd' a)
 #-}

sliceUSegd :: USegd -> Int -> Int -> USegd
sliceUSegd segd i n = zipU lens idxs'
  where
    lens  = sliceU (lengthsUSegd segd) i n
    idxs  = sliceU (indicesUSegd segd) i n
    idxs' = mapU (subtract k) idxs
    k     = idxs !: 0

extractUSegd :: USegd -> Int -> Int -> USegd
extractUSegd segd i n = zipU lens idxs'
  where
    lens  = extractU (lengthsUSegd segd) i n
    idxs  = sliceU   (indicesUSegd segd) i n
    idxs' = mapU (subtract k) idxs
    k     = idxs !: 0

