{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPSegd (
  -- * Types
  UPSegd, validUPSegd,

  -- * Constructors
  mkUPSegd, emptyUPSegd, singletonUPSegd,
  lengthsToUPSegd, 
  
  -- * Projections
  lengthUPSegd, lengthsUPSegd, indicesUPSegd, elementsUPSegd,
  segdUPSegd, distUPSegd,
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Distributed

-- | A Parallel segment descriptor holds the original descriptor,
--   and a distributed one that describes how to distribute the work
--   on such a segmented array.
data UPSegd 
        = UPSegd 
        { upsegd_usegd :: !USegd
          -- ^ Segment descriptor that describes the whole array.

        , upsegd_dsegd :: Dist ((USegd,Int),Int)
          -- ^ Segment descriptor for each chunk, 
          --   along with segment id of first slice in the chunk,
          --   and the offset of that slice in its segment.
          --   See docs of `splitSegdOfElemsD` for an example.
        }


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
--   TODO: doesn't do any checks yet
validUPSegd :: UPSegd -> Bool
{-# INLINE validUPSegd #-}
validUPSegd _ = True


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUPSegd 
        :: Vector Int   -- ^ length of each segment
        -> Vector Int   -- ^ starting index of each segment
        -> Int          -- ^ total number of elements in the flat array
        -> UPSegd

{-# INLINE mkUPSegd #-}
mkUPSegd lens idxs n = toUPSegd (mkUSegd lens idxs n)


-- | O(1).
--  Convert a global `USegd` to a distributed `UPSegd` by splitting
--  it across the gang.
toUPSegd :: USegd -> UPSegd
{-# INLINE toUPSegd #-}
toUPSegd segd = UPSegd segd (splitSegdOnElemsD theGang segd)


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
emptyUPSegd :: UPSegd
{-# INLINE emptyUPSegd #-}
emptyUPSegd = toUPSegd emptyUSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singletonUPSegd :: Int -> UPSegd
{-# INLINE singletonUPSegd #-}
singletonUPSegd n = toUPSegd $ singletonUSegd n


-- | O(n). Convert a length array into a segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that. Runtime is O(n) in the number of segments.
--
lengthsToUPSegd :: Vector Int -> UPSegd
{-# INLINE lengthsToUPSegd #-}
lengthsToUPSegd = toUPSegd . lengthsToUSegd


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
lengthUPSegd :: UPSegd -> Int
{-# INLINE lengthUPSegd #-}
lengthUPSegd = lengthUSegd . upsegd_usegd


-- | O(1). Yield the lengths of the individual segments.
lengthsUPSegd :: UPSegd -> Vector Int
{-# INLINE lengthsUPSegd #-}
lengthsUPSegd = lengthsUSegd . upsegd_usegd


-- | O(1). Yield the segment indices of a segment descriptor.
indicesUPSegd :: UPSegd -> Vector Int
{-# INLINE indicesUPSegd #-}
indicesUPSegd = indicesUSegd . upsegd_usegd


-- | O(1). Yield the number of data elements.
elementsUPSegd :: UPSegd -> Int
{-# INLINE elementsUPSegd #-}
elementsUPSegd = elementsUSegd . upsegd_usegd


-- | O(1). Yield the global `USegd` of a `UPSegd`
segdUPSegd :: UPSegd -> USegd
{-# INLINE segdUPSegd #-}
segdUPSegd = upsegd_usegd


-- | O(1). Yield the distributed `USegd` of a `UPSegd`
distUPSegd :: UPSegd -> Dist ((USegd,Int),Int)
{-# INLINE distUPSegd #-}
distUPSegd = upsegd_dsegd

