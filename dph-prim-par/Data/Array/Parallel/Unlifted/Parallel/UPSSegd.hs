{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPSSegd (
  -- * Types
  UPSSegd, validUPSSegd,

  -- * Constructors
  mkUPSSegd, emptyUPSSegd, singletonUPSSegd,
  promoteUPSegdToUPSSegd,
  
  -- * Projections
  lengthUPSSegd, lengthsUPSSegd, indicesUPSSegd, elementsUPSSegd,
  startsUPSSegd, sourcesUPSSegd,
  getSegOfUPSSegd,
  ssegdUPSSegd, distUPSSegd,
  
  -- * Operators
  appendUPSSegd
  
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Sequential.USegd
import Data.Array.Parallel.Unlifted.Sequential.USSegd
import Data.Array.Parallel.Unlifted.Parallel.UPSegd
import Data.Array.Parallel.Unlifted.Distributed

-- | A Parallel segment descriptor holds the original descriptor,
--   and a distributed one that describes how to distribute the work
--   on such a segmented array.
data UPSSegd 
        = UPSSegd 
        { upssegd_ussegd :: !USSegd
          -- ^ Segment descriptor that describes the whole array.

        , upssegd_dssegd :: Dist ((USSegd,Int),Int)
          -- ^ Segment descriptor for each chunk, 
          --   along with segment id of first slice in the chunk,
          --   and the offset of that slice in its segment.
          --   See docs of `splitSegdOfElemsD` for an example.
        }


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
--   TODO: doesn't do any checks yet
validUPSSegd :: UPSSegd -> Bool
{-# INLINE validUPSSegd #-}
validUPSSegd _ = True


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUPSSegd 
        :: Vector Int   -- ^ starting index of each segment in its flat array
        -> Vector Int   -- ^ which array to take each segment from
        -> USegd        -- ^ contiguous segment descriptor
        -> UPSSegd

{-# INLINE mkUPSSegd #-}
mkUPSSegd starts sources usegd
        = toUPSSegd (mkUSSegd starts sources usegd)


-- | O(1).
--  Convert a global `USegd` to a distributed `UPSegd` by splitting
--  it across the gang.
toUPSSegd :: USSegd -> UPSSegd
{-# INLINE toUPSSegd #-}
toUPSSegd ssegd 
        = UPSSegd ssegd (splitSSegdOnElemsD theGang ssegd)


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
emptyUPSSegd :: UPSSegd
{-# INLINE emptyUPSSegd #-}
emptyUPSSegd
        = toUPSSegd emptyUSSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singletonUPSSegd :: Int -> UPSSegd
{-# INLINE singletonUPSSegd #-}
singletonUPSSegd n  = toUPSSegd $ singletonUSSegd n


-- | O(segs). 
--   Promote a plain USSegd to a UPSSegd
--   All segments are assumed to come from a flat array with sourceid 0.

--   TODO: Sequential construction of the indices and source field.
--         We throw out the existing distributed usegd here,
--          maybe we can do the promotion while keeping some of the existing fields.
promoteUPSegdToUPSSegd :: UPSegd -> UPSSegd
{-# INLINE promoteUPSegdToUPSSegd #-}
promoteUPSegdToUPSSegd upsegd
 = toUPSSegd $ promoteUSegdToUSSegd $ segdUPSegd upsegd


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
lengthUPSSegd :: UPSSegd -> Int
{-# INLINE lengthUPSSegd #-}
lengthUPSSegd = lengthUSSegd . upssegd_ussegd


-- | O(1). Yield the global `USegd` of a `UPSegd`
ssegdUPSSegd :: UPSSegd -> USSegd
{-# INLINE ssegdUPSSegd #-}
ssegdUPSSegd = upssegd_ussegd


-- | O(1). Yield the distributed `USegd` of a `UPSegd`
distUPSSegd :: UPSSegd -> Dist ((USSegd, Int), Int)
{-# INLINE distUPSSegd #-}
distUPSSegd = upssegd_dssegd


-- | O(1). Yield the lengths of the individual segments.
lengthsUPSSegd :: UPSSegd -> Vector Int
{-# INLINE lengthsUPSSegd #-}
lengthsUPSSegd = lengthsUSSegd . upssegd_ussegd


-- | O(1). Yield the segment indices of a segment descriptor.
indicesUPSSegd :: UPSSegd -> Vector Int
{-# INLINE indicesUPSSegd #-}
indicesUPSSegd = indicesUSSegd . upssegd_ussegd


-- | O(1). Yield the number of data elements.
elementsUPSSegd :: UPSSegd -> Int
{-# INLINE elementsUPSSegd #-}
elementsUPSSegd = elementsUSSegd . upssegd_ussegd


-- | O(1). Yield the starting indices of a `UPSSegd`
startsUPSSegd :: UPSSegd -> Vector Int
{-# INLINE startsUPSSegd #-}
startsUPSSegd = startsUSSegd . upssegd_ussegd


-- | O(1). Yield the source ids of a `UPSSegd`
sourcesUPSSegd :: UPSSegd -> Vector Int
{-# INLINE sourcesUPSSegd #-}
sourcesUPSSegd = sourcesUSSegd . upssegd_ussegd 


-- | O(1).
--   Get the length, segment index, starting index, and source id of a segment.
getSegOfUPSSegd :: UPSSegd -> Int -> (Int, Int, Int, Int)
{-# INLINE getSegOfUPSSegd #-}
getSegOfUPSSegd upssegd ix
        = getSegOfUSSegd (upssegd_ussegd upssegd) ix


-- Operators ------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor that describes the result of appending.
--   
--   TODO: This calls out to the sequential version.
--
appendUPSSegd 
        :: UPSSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> UPSSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> UPSSegd
{-# INLINE appendUPSSegd #-}
appendUPSSegd
        upssegd1 pdatas1
        upssegd2 pdatas2
 = toUPSSegd 
 $ appendUSSegd (upssegd_ussegd upssegd1) pdatas1
                (upssegd_ussegd upssegd2) pdatas2

