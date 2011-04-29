-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--             , (c) 2006 Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Segment Descriptors
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (
  -- * Types
  USegd,

  -- * Constructors
  mkUSegd,
  emptyUSegd, singletonUSegd, lengthsToUSegd,

  -- * Projections
  lengthUSegd, lengthsUSegd, indicesUSegd, elementsUSegd, 

  -- * Operations
  sliceUSegd, extractUSegd
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as V

-- | Segment descriptors represent the structure of nested arrays.
--  For each segment, it stores the length and the starting index in the flat data array.
--
--   Example:
--
--   @
--    flat array data:  [1, 2, 3, 4, 5, 6, 7, 8]
--      (segmentation)   ----  -------  -  ----
--.
--      segd  lengths: [2, 3, 1, 2]
--            indices: [0, 2, 5, 6]
--           elements: 8 
--   @
data USegd 
        = USegd 
        { usegd_lengths  :: !(Vector Int)  -- ^ length of each segment
        , usegd_indices  :: !(Vector Int)  -- ^ starting index of each segment in the flat array
        , usegd_elements :: !Int           -- ^ total number of elements in the flat array
        }


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUSegd 
        :: Vector Int   -- ^ length of each segment
        -> Vector Int   -- ^ starting index of each segment
        -> Int          -- ^ total number of elements in the flat array
        -> USegd

{-# INLINE mkUSegd #-}
mkUSegd = USegd


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
emptyUSegd :: USegd
{-# INLINE emptyUSegd #-}
emptyUSegd = USegd V.empty V.empty 0


-- | O(1). Yield a singleton segment descriptor.
--         The single segment covers the given number of elements.
singletonUSegd :: Int -> USegd
{-# INLINE singletonUSegd #-}
singletonUSegd n = USegd (V.singleton n) (V.singleton 0) n


-- | O(n). Convert a length array into a segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that. Runtime is O(n) in the number of segments.
--
lengthsToUSegd :: Vector Int -> USegd
{-# INLINE lengthsToUSegd #-}
lengthsToUSegd lens
        = USegd lens (V.scanl (+) 0 lens) (V.sum lens)


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
lengthUSegd :: USegd -> Int
{-# INLINE lengthUSegd #-}
lengthUSegd = V.length . usegd_lengths


-- | O(1). Yield the lengths of the individual segments.
lengthsUSegd :: USegd -> Vector Int
{-# INLINE lengthsUSegd #-}
lengthsUSegd = usegd_lengths


-- | O(1). Yield the segment indices of a segment descriptor.
indicesUSegd :: USegd -> Vector Int
{-# INLINE indicesUSegd #-}
indicesUSegd = usegd_indices


-- | O(1). Yield the number of data elements.
elementsUSegd :: USegd -> Int
{-# INLINE elementsUSegd #-}
elementsUSegd = usegd_elements


-- | O(n). Extract a slice of a segment descriptor, avoiding copying where possible.
--
--   We can share the segment lengths with the original segment descriptor, 
--   but still need to recompute the starting indices of each. Hence
--   runtime is O(n) in the number of segments sliced out.
-- 
--   NOTE: In the new segment descriptor, the starting index of the first
--         segment will be 0.
sliceUSegd 
        :: USegd        -- ^ source segment descriptor
        -> Int          -- ^ index of first segment
        -> Int          -- ^ number of segments to slice out
        -> USegd
        
{-# INLINE sliceUSegd #-}
sliceUSegd segd i n
        = lengthsToUSegd $ V.slice (lengthsUSegd segd) i n


-- | O(n). Extract a slice of a segment descriptor, copying everything.
--
--   In contrast to `sliceUSegd`, this function copies the array of 
--   segment lengths as well as recomputing the starting indices of each.
--
--   NOTE: In the new segment descriptor, the starting index of the first
--         segment will be 0.
extractUSegd 
        :: USegd        -- ^ source segment desciptor
        -> Int          -- ^ index of the first segment
        -> Int          -- ^ number of segments to extract out
        -> USegd

{-# INLINE extractUSegd #-}
extractUSegd segd i n 
        = lengthsToUSegd $ V.extract (lengthsUSegd segd) i n

