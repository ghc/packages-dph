
-- | Segment descriptors for virtual arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.UVSegd (
        -- * Virtual Segment Descriptors
        UVSegd(..),
        lengthUVSegd,
        lengthsUVSegd,
        updateVSegsOfUVSegd,
        unsafeMaterializeUVSegd,

        -- * Types
        USSegd(..),
        
        -- * Constructors
        mkUSSegd,
        emptyUSSegd,
        singletonUSSegd,
        promoteUSegdToUSSegd,
        
        -- * Projections
        lengthUSSegd,
        lengthsUSSegd,
        indicesUSSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector as V

-- UVSegd ---------------------------------------------------------------------
-- TODO shift this to its own module
-- | Defines a virutal nested array based on physical segmentation.
data UVSegd 
        = UVSegd 
        { uvsegd_vsegids :: !(Vector Int) 
          -- ^ array saying which physical segment to use for each virtual segment 

        , uvsegd_ussegd  :: !USSegd
          -- ^ slice segment descriptor describing physical segments.
        }
        deriving (Show)


lengthUVSegd :: UVSegd -> Int
lengthUVSegd (UVSegd vsegids _)
        = V.length vsegids

-- | O(segs). Yield the lengths of the individual segments.
lengthsUVSegd :: UVSegd -> Vector Int
{-# INLINE lengthsUVSegd #-}
lengthsUVSegd (UVSegd vsegids ussegd)
        = V.map ((ussegd_lengths ussegd) V.!) vsegids


-- | TODO: automatically force out unreachable psegs here.
updateVSegsOfUVSegd :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegsOfUVSegd f (UVSegd vsegids ussegd)
        = UVSegd (f vsegids) ussegd

-- | O(segs)
--   Given an virtual segment descriptor, produce a plain USegd that
--   that describes the entire array.
--
--   WARNING:
--   Trying to take the USegd of a nested array that has been constructed with
--   replication can cause index overflow. This is because the virtual size of
--   the corresponding flat data can be larger than physical memory.
-- 
--   You should only apply this function to a nested array when you're about
--   about to construct something with the same size as the corresponding
--   flat array. In this case the index overflow doesn't matter too much
--   because the program would OOM anyway.
--
unsafeMaterializeUVSegd :: UVSegd -> USegd
unsafeMaterializeUVSegd (UVSegd vsegids ussegd)
        = lengthsToUSegd 
        $ V.bpermute (lengthsUSSegd ussegd) vsegids



-- USSegd ---------------------------------------------------------------------
-- | Slice segment descriptors are a generalisation of regular 'physical'
--   segment descriptors of type (Segd). 
--   
--   * SSegd segments may be drawn from multiple physical source arrays.
--   * The segments need not cover the entire flat array.
--   * Different segments may not cover the same elements.
--
data USSegd
        = USSegd
        { ussegd_lengths :: !(Vector Int)
          -- ^ length of each segment

        , ussegd_indices :: !(Vector Int)
          -- ^ starting index of each segment in its flat array

        , ussegd_srcids  :: !(Vector Int)
          -- ^ which flat array to take each segment from.
        }
        deriving (Show)

-- Constructors ---------------------------------------------------------------
-- | O(1). 
--   Construct a new slice segment descriptor.
--   All the provided arrays must have the same lengths.
mkUSSegd
        :: Vector Int   -- ^ length of each segment
        -> Vector Int   -- ^ starting index of each segment in its flat array
        -> Vector Int   -- ^ which flat array to take each segment from
        -> USSegd

{-# INLINE mkUSSegd #-}
mkUSSegd = USSegd


-- | O(1).
--   Check the internal consistency of a slice segment descriptor.
--   TODO: check that segments don't overlap.
validUSSegd :: USSegd -> Bool
{-# INLINE validUSSegd #-}
validUSSegd (USSegd lens starts srcids)
        =  (V.length lens == V.length starts)
        && (V.length lens == V.length srcids)


-- | O(1).
--  Yield an empty segment descriptor, with no elements or segments.
emptyUSSegd :: USSegd
{-# INLINE emptyUSSegd #-}
emptyUSSegd = USSegd V.empty V.empty V.empty


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singletonUSSegd :: Int -> USSegd
{-# INLINE singletonUSSegd #-}
singletonUSSegd n 
        = USSegd (V.singleton n) (V.singleton 0) (V.singleton 0)


-- | O(segs). 
--   Promote a plain USegd to a USSegd
--      All segments are assumed to come from a flat array with sourceid 0.
promoteUSegdToUSSegd :: USegd -> USSegd
{-# INLINE promoteUSegdToUSSegd #-}
promoteUSegdToUSSegd usegd
        = USSegd (lengthsUSegd usegd)
                 (indicesUSegd usegd)
                 (V.replicate (lengthUSegd usegd) 0)


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
lengthUSSegd :: USSegd -> Int
{-# INLINE lengthUSSegd #-}
lengthUSSegd = V.length . ussegd_lengths


-- | O(1). Yield the lengths of the individual segments.
lengthsUSSegd :: USSegd -> Vector Int
{-# INLINE lengthsUSSegd #-}
lengthsUSSegd = ussegd_lengths


-- | O(1). Yield the segment indices of a segment descriptor.
indicesUSSegd :: USSegd -> Vector Int
{-# INLINE indicesUSSegd #-}
indicesUSSegd = ussegd_indices


-- | O(segs). 
--   Yield the total number of flat data elements covered by the segment
--   descriptor. This is the sum of all the segment length fields.
--   TODO: is O(segs) ok? Where do we use this function?
elementsUSSegd :: USSegd -> Int
{-# INLINE elementsUSSegd #-}
elementsUSSegd = V.sum . ussegd_lengths 




