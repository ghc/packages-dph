
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
        appendUSSegd,
        cullUSSegdOnVSegids
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


-- | Cull the segments in a SSegd down to only those reachable from an array
--   of vsegids, and also update the vsegids to point to the same segments
--   in the result.
--
--   TODO: bpermuteDft isn't parallelised
--
cullUSSegdOnVSegids :: Vector Int -> USSegd -> (Vector Int, USSegd)
{-# INLINE cullUSSegdOnVSegids #-}
cullUSSegdOnVSegids vsegids (USSegd lengths indices srcids)
 = let  -- Determine which of the psegs are still reachable from the vsegs.
        -- This produces an array of flags, 
        --    with reachable   psegs corresponding to 1
        --    and  unreachable psegs corresponding to 0
        -- 
        --  eg  vsegids:        [0 1 1 3 5 5 6 6]
        --   => psegids_used:   [1 1 0 1 0 1 1]
        --  
        --  Note that psegids '2' and '4' are not in vsegids_packed.
        psegids_used
         = V.bpermuteDft (V.length lengths)
                         (const False)
                         (V.zip vsegids (V.replicate (V.length vsegids) True))

        -- Produce an array of used psegs.
        --  eg  psegids_used:   [1 1 0 1 0 1 1]
        --      psegids_packed: [0 1 3 5 6]
        psegids_packed
         = V.pack (V.enumFromTo 0 (V.length psegids_used)) psegids_used

        -- Produce an array that maps psegids in the source array onto
        -- psegids in the result array. If a particular pseg isn't present
        -- in the result this maps onto -1.

        --  Note that if psegids_used has 0 in some position, then psegids_map
        --  has -1 in the same position, corresponding to an unused pseg.
         
        --  eg  psegids_packed: [0 1 3 5 6]
        --                      [0 1 2 3 4]
        --      psegids_map:    [0 1 -1 2 -1 3 4]
        psegids_map
         = V.bpermuteDft (V.length lengths)
                         (const (-1))
                         (V.zip psegids_packed (V.enumFromTo 0 (V.length psegids_packed - 1)))

        -- Use the psegids_map to rewrite the packed vsegids to point to the 
        -- corresponding psegs in the result.
        -- 
        --  eg  vsegids:        [0 1 1 3 5 5 6 6]
        --      psegids_map:    [0 1 -1 2 -1 3 4]
        -- 
        --      vsegids':       [0 1 1 2 3 3 4 4]
        --
        vsegids'  = V.map (psegids_map V.!) vsegids

        ussegd'   = USSegd (V.pack lengths psegids_used)
                           (V.pack indices psegids_used)
                           (V.pack srcids  psegids_used)

     in  (vsegids', ussegd')
