-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
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

module Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (

  -- * Types
  USegd,

  -- * Operations on segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd, elementsUSegd, mkUSegd,
  emptyUSegd, singletonUSegd, lengthsToUSegd,
  sliceUSegd, extractUSegd
) where

import Data.Array.Parallel.Base (
  (:*:)(..), ST)
import Data.Array.Parallel.Unlifted.Sequential.Flat (
  UArr, MUArr,
  lengthU, emptyU, singletonU, sliceU, extractU,
  scanlU, sumU )

import Control.Monad (
  liftM)


-- | Segment descriptors represent the structure of nested arrays. For each
-- segment, it stores the length and the starting index in the flat data
-- array.
--
data USegd = USegd { usegd_lengths  :: !(UArr Int)
                   , usegd_indices  :: !(UArr Int)
                   , usegd_elements :: !Int
                   }

-- |Operations on segment descriptors
-- ----------------------------------

-- |Yield the overall number of segments
--
lengthUSegd :: USegd -> Int
{-# INLINE lengthUSegd #-}
lengthUSegd = lengthU . usegd_lengths

-- |Yield the segment lengths of a segment descriptor
--
lengthsUSegd :: USegd -> UArr Int
{-# INLINE lengthsUSegd #-}
lengthsUSegd = usegd_lengths

-- |Yield the segment indices of a segment descriptor
--
indicesUSegd :: USegd -> UArr Int
{-# INLINE indicesUSegd #-}
indicesUSegd = usegd_indices

-- |Yield the number of data elements
--
elementsUSegd :: USegd -> Int
{-# INLINE elementsUSegd #-}
elementsUSegd = usegd_elements

mkUSegd :: UArr Int -> UArr Int -> Int -> USegd
{-# INLINE mkUSegd #-}
mkUSegd = USegd

-- |Yield an empty segment descriptor
--
emptyUSegd :: USegd
{-# INLINE emptyUSegd #-}
emptyUSegd = USegd emptyU emptyU 0

-- |Yield a singleton segment descriptor
--
singletonUSegd :: Int -> USegd
{-# INLINE singletonUSegd #-}
singletonUSegd n = USegd (singletonU n) (singletonU 0) n

-- |Convert a length array into a segment descriptor.
--
lengthsToUSegd :: UArr Int -> USegd
{-# INLINE lengthsToUSegd #-}
lengthsToUSegd lens = USegd lens (scanlU (+) 0 lens) (sumU lens)

-- |Extract a slice of a segment descriptor, avoiding copying where possible.
--
-- NOTE: In the new segment descriptor, the starting index of the first
--       segment will be 0.
--
sliceUSegd :: USegd -> Int -> Int -> USegd
{-# INLINE sliceUSegd #-}
sliceUSegd segd i n = lengthsToUSegd $ sliceU (lengthsUSegd segd) i n

-- |Extract a slice of a segment descriptor, copying everything.
--
-- NOTE: In the new segment descriptor, the starting index of the first
--       segment will be 0.
--
extractUSegd :: USegd -> Int -> Int -> USegd
{-# INLINE extractUSegd #-}
extractUSegd segd i n = lengthsToUSegd $ extractU (lengthsUSegd segd) i n

