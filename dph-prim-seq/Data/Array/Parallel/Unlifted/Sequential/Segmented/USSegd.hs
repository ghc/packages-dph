{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
-- | Scatter Segment Descriptors
module Data.Array.Parallel.Unlifted.Sequential.Segmented.USSegd (
        -- * Types
        USSegd(..),
        
        -- * Consistency check
        validUSSegd,

        -- * Constructors
        mkUSSegd,
        emptyUSSegd,
        singletonUSSegd,
        promoteUSegdToUSSegd,
        
        -- * Projections
        lengthUSSegd,
        lengthsUSSegd, indicesUSSegd,
        sourcesUSSegd, startsUSSegd,
        usegdUSSegd,
        getSegOfUSSegd,
        
        -- * Operators
        appendUSSegd,
        cullUSSegdOnVSegids
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Pretty


-- USSegd ---------------------------------------------------------------------
-- | Scatter segment descriptors are a generalisation of regular 
--   segment descriptors of type (Segd). 
--   
--   * SSegd segments may be drawn from multiple physical source arrays.
--   * The segments need not cover the entire flat array.
--   * Different segments may point to the same elements.
--
--   * As different segments may point to the same elements, it is possible
--     for the total number of elements covered by the segment descriptor
--     to overflow a machine word.
-- 
data USSegd
        = USSegd
        { ussegd_starts  :: !(Vector Int)
          -- ^ starting index of each segment in its flat array

        , ussegd_sources :: !(Vector Int)
          -- ^ which flat array to take each segment from.

        , ussegd_usegd   :: !USegd
          -- ^ segment descriptor with contiguous index space.
        }
        deriving (Show)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical USSegd where
 pprp (USSegd starts srcids ssegd)
  = vcat
  [ text "USSegd" 
        $$ (nest 7 $ vcat
                [ text "starts:  " <+> (text $ show $ V.toList starts)
                , text "srcids:  " <+> (text $ show $ V.toList srcids) ])
  , pprp ssegd ]


-- Constructors ---------------------------------------------------------------
-- | O(1). 
--   Construct a new scattered segment descriptor.
--   All the provided arrays must have the same lengths.
mkUSSegd
        :: Vector Int   -- ^ starting index of each segment in its flat array
        -> Vector Int   -- ^ which array to take each segment from
        -> USegd        -- ^ contiguous segment descriptor
        -> USSegd

{-# INLINE mkUSSegd #-}
mkUSSegd = USSegd


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
validUSSegd :: USSegd -> Bool
{-# INLINE validUSSegd #-}
validUSSegd (USSegd starts srcids usegd)
        =  (V.length starts == lengthUSegd usegd)
        && (V.length srcids == lengthUSegd usegd)


-- | O(1).
--  Yield an empty segment descriptor, with no elements or segments.
emptyUSSegd :: USSegd
{-# INLINE emptyUSSegd #-}
emptyUSSegd = USSegd V.empty V.empty emptyUSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singletonUSSegd :: Int -> USSegd
{-# INLINE singletonUSSegd #-}
singletonUSSegd n 
        = USSegd (V.singleton 0) (V.singleton 0) (singletonUSegd n)


-- | O(segs). 
--   Promote a plain USegd to a USSegd
--   All segments are assumed to come from a flat array with sourceid 0.
promoteUSegdToUSSegd :: USegd -> USSegd
{-# INLINE promoteUSegdToUSSegd #-}
promoteUSegdToUSSegd usegd
        = USSegd (indicesUSegd usegd)
                 (V.replicate (lengthUSegd usegd) 0)
                 usegd
                 


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
lengthUSSegd :: USSegd -> Int
{-# INLINE lengthUSSegd #-}
lengthUSSegd = lengthUSegd . ussegd_usegd 


-- | O(1). Yield the lengths of the segments of a `USSegd`
lengthsUSSegd :: USSegd -> Vector Int
{-# INLINE lengthsUSSegd #-}
lengthsUSSegd = lengthsUSegd . ussegd_usegd


-- | O(1). Yield the segment indices of a `USSegd`
indicesUSSegd :: USSegd -> Vector Int
{-# INLINE indicesUSSegd #-}
indicesUSSegd = indicesUSegd . ussegd_usegd


-- | O(1). Yield the starting indices of a `USSegd`
startsUSSegd :: USSegd -> Vector Int
{-# INLINE startsUSSegd #-}
startsUSSegd = ussegd_starts


-- | O(1). Yield the source ids of a `USSegd`
sourcesUSSegd :: USSegd -> Vector Int
{-# INLINE sourcesUSSegd #-}
sourcesUSSegd = ussegd_sources


-- | O(1). Yield the `USegd` of a `USSegd`
usegdUSSegd   :: USSegd -> USegd
{-# INLINE usegdUSSegd #-}
usegdUSSegd   = ussegd_usegd


-- | O(1).
--   Get the length, segment index, starting index, and source id of a segment.
getSegOfUSSegd :: USSegd -> Int -> (Int, Int, Int, Int)
getSegOfUSSegd (USSegd starts sources usegd) ix
 = let  (len, index) = getSegOfUSegd usegd ix
   in   ( len
        , index
        , starts  V.! ix
        , sources V.! ix)


-- Operators ------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor that describes the result of appending
--   two arrays.
appendUSSegd 
        :: USSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> USSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> USSegd

appendUSSegd (USSegd starts1 srcs1 usegd1) pdatas1
             (USSegd starts2 srcs2 usegd2) _
        = USSegd (starts1  V.++  starts2)
                 (srcs1    V.++  V.map (+ pdatas1) srcs2)
                 (appendUSegd usegd1 usegd2)


-- | Cull the segments in a SSegd down to only those reachable from an array
--   of vsegids, and also update the vsegids to point to the same segments
--   in the result.
--
--   TODO: bpermuteDft isn't parallelised
--
cullUSSegdOnVSegids :: Vector Int -> USSegd -> (Vector Int, USSegd)
{-# INLINE cullUSSegdOnVSegids #-}
cullUSSegdOnVSegids vsegids (USSegd starts sources usegd)
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
         = V.bpermuteDft (lengthUSegd usegd)
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
         = V.bpermuteDft (lengthUSegd usegd)
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

        -- Rebuild the usegd.
        starts'   = V.pack starts  psegids_used
        sources'  = V.pack sources psegids_used

        lengths'  = V.pack (lengthsUSegd usegd) psegids_used
        usegd'    = lengthsToUSegd lengths'
        
        ussegd'   = USSegd starts' sources' usegd'

     in  (vsegids', ussegd')




