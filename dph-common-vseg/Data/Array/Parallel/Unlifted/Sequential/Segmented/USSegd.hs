
-- | Segment descriptors for virtual arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.USSegd (
        -- * Types
        USSegd(..),
        
        -- * Constructors
        mkUSSegd,
        emptyUSSegd,
        singletonUSSegd,
        promoteUSegdToUSSegd,
        
        -- * Projections
        lengthUSSegd,
        lengthsUSSegd, indicesUSSegd, sourcesUSSegd,
        getSegOfUSSegd,
        
        -- * Operators
        appendUSSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector as V


-- USSegd ---------------------------------------------------------------------
-- | Slice segment descriptors are a generalisation of regular 'physical'
--   segment descriptors of type (Segd). 
--   
--   * SSegd segments may be drawn from multiple physical source arrays.
--   * The segments need not cover the entire flat array.
--   * Different segments may point to the same elements.
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
--   All segments are assumed to come from a flat array with sourceid 0.
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


-- | O(1). Yield the lengths of the segments of a `USSegd`
lengthsUSSegd :: USSegd -> Vector Int
{-# INLINE lengthsUSSegd #-}
lengthsUSSegd = ussegd_lengths


-- | O(1). Yield the segment indices of a `USSegd`
indicesUSSegd :: USSegd -> Vector Int
{-# INLINE indicesUSSegd #-}
indicesUSSegd = ussegd_indices


-- | O(1). Yield the source ids of a slice segment descriptor.
sourcesUSSegd :: USSegd -> Vector Int
{-# INLINE sourcesUSSegd #-}
sourcesUSSegd = ussegd_srcids


-- | O(segs). 
--   Yield the total number of flat data elements covered by the segment
--   descriptor. This is the sum of all the segment length fields.
--   TODO: is O(segs) ok? Where do we use this function?
elementsUSSegd :: USSegd -> Int
{-# INLINE elementsUSSegd #-}
elementsUSSegd = V.sum . ussegd_lengths 


-- O(1).
-- Get the length, starting index, and source id of a segment.
getSegOfUSSegd :: Int -> USSegd -> (Int, Int, Int)
getSegOfUSSegd ix (USSegd lengths starts sourceids)
 =      ( lengths   V.! ix
        , starts    V.! ix
        , sourceids V.! ix)


-- Operators ------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor describing the result of appending
--   two arrays.
appendUSSegd 
        :: USSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> USSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> USSegd

appendUSSegd (USSegd lens1 starts1 srcs1) pdatas1
             (USSegd lens2 starts2 srcs2) _
        = USSegd (lens1    V.++  lens2)
                 (starts1  V.++  starts2)
                 (srcs1    V.++  V.map (+ pdatas1) srcs2)

