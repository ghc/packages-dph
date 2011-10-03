{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
-- | Scattered Segment Descriptors
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
        usegdUSSegd, lengthsUSSegd, indicesUSSegd, elementsUSSegd,
        sourcesUSSegd, startsUSSegd,
        getSegOfUSSegd,
        
        -- * Operators
        appendUSSegd,
        cullUSSegdOnVSegids,
        
        -- * Streams
        streamSegsFromUSSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import qualified Data.Vector                                    as V
import qualified Data.Vector.Fusion.Stream                      as S
import qualified Data.Vector.Fusion.Stream.Size                 as S
import qualified Data.Vector.Fusion.Stream.Monadic              as M
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
                [ text "starts:  " <+> (text $ show $ U.toList starts)
                , text "srcids:  " <+> (text $ show $ U.toList srcids) ])
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
        =  (U.length starts == lengthUSegd usegd)
        && (U.length srcids == lengthUSegd usegd)


-- | O(1).
--  Yield an empty segment descriptor, with no elements or segments.
emptyUSSegd :: USSegd
{-# INLINE emptyUSSegd #-}
emptyUSSegd = USSegd U.empty U.empty emptyUSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singletonUSSegd :: Int -> USSegd
{-# INLINE singletonUSSegd #-}
singletonUSSegd n 
        = USSegd (U.singleton 0) (U.singleton 0) (singletonUSegd n)


-- | O(segs). 
--   Promote a plain USegd to a USSegd
--   All segments are assumed to come from a flat array with sourceid 0.
promoteUSegdToUSSegd :: USegd -> USSegd
{-# INLINE promoteUSegdToUSSegd #-}
promoteUSegdToUSSegd usegd
        = USSegd (indicesUSegd usegd)
                 (U.replicate (lengthUSegd usegd) 0)
                 usegd
                 


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
lengthUSSegd :: USSegd -> Int
{-# INLINE lengthUSSegd #-}
lengthUSSegd = lengthUSegd . ussegd_usegd 


-- | O(1). Yield the `USegd` of a `USSegd`
usegdUSSegd   :: USSegd -> USegd
{-# INLINE usegdUSSegd #-}
usegdUSSegd   = ussegd_usegd


-- | O(1). Yield the lengths of the segments of a `USSegd`
lengthsUSSegd :: USSegd -> Vector Int
{-# INLINE lengthsUSSegd #-}
lengthsUSSegd = lengthsUSegd . ussegd_usegd


-- | O(1). Yield the segment indices of a `USSegd`
indicesUSSegd :: USSegd -> Vector Int
{-# INLINE indicesUSSegd #-}
indicesUSSegd = indicesUSegd . ussegd_usegd


-- | O(1). Yield the total number of elements covered by a `USSegd`
elementsUSSegd :: USSegd -> Int
{-# INLINE elementsUSSegd #-}
elementsUSSegd = elementsUSegd . ussegd_usegd


-- | O(1). Yield the starting indices of a `USSegd`
startsUSSegd :: USSegd -> Vector Int
{-# INLINE startsUSSegd #-}
startsUSSegd = ussegd_starts


-- | O(1). Yield the source ids of a `USSegd`
sourcesUSSegd :: USSegd -> Vector Int
{-# INLINE sourcesUSSegd #-}
sourcesUSSegd = ussegd_sources


-- | O(1).
--   Get the length, segment index, starting index, and source id of a segment.
getSegOfUSSegd :: USSegd -> Int -> (Int, Int, Int, Int)
getSegOfUSSegd (USSegd starts sources usegd) ix
 = let  (len, index) = getSegOfUSegd usegd ix
   in   ( len
        , index
        , starts  U.! ix
        , sources U.! ix)


-- Operators ------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor that describes the result of appending
--   two arrays.
appendUSSegd 
        :: USSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> USSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> USSegd
{-# INLINE appendUSSegd #-}
appendUSSegd (USSegd starts1 srcs1 usegd1) pdatas1
             (USSegd starts2 srcs2 usegd2) _
        = USSegd (starts1  U.++  starts2)
                 (srcs1    U.++  U.map (+ pdatas1) srcs2)
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
         = U.bpermuteDft (lengthUSegd usegd)
                         (const False)
                         (U.zip vsegids (U.replicate (U.length vsegids) True))

        -- Produce an array of used psegs.
        --  eg  psegids_used:   [1 1 0 1 0 1 1]
        --      psegids_packed: [0 1 3 5 6]
        psegids_packed
         = U.pack (U.enumFromTo 0 (U.length psegids_used)) psegids_used

        -- Produce an array that maps psegids in the source array onto
        -- psegids in the result array. If a particular pseg isn't present
        -- in the result this maps onto -1.

        --  Note that if psegids_used has 0 in some position, then psegids_map
        --  has -1 in the same position, corresponding to an unused pseg.
         
        --  eg  psegids_packed: [0 1 3 5 6]
        --                      [0 1 2 3 4]
        --      psegids_map:    [0 1 -1 2 -1 3 4]
        psegids_map
         = U.bpermuteDft (lengthUSegd usegd)
                         (const (-1))
                         (U.zip psegids_packed (U.enumFromTo 0 (U.length psegids_packed - 1)))

        -- Use the psegids_map to rewrite the packed vsegids to point to the 
        -- corresponding psegs in the result.
        -- 
        --  eg  vsegids:        [0 1 1 3 5 5 6 6]
        --      psegids_map:    [0 1 -1 2 -1 3 4]
        -- 
        --      vsegids':       [0 1 1 2 3 3 4 4]
        --
        vsegids'  = U.map (psegids_map U.!) vsegids

        -- Rebuild the usegd.
        starts'   = U.pack starts  psegids_used
        sources'  = U.pack sources psegids_used

        lengths'  = U.pack (lengthsUSegd usegd) psegids_used
        usegd'    = lengthsToUSegd lengths'
        
        ussegd'   = USSegd starts' sources' usegd'

     in  (vsegids', ussegd')



-- | Stream some physical segments from many data arrays.
--   TODO: make this more efficient, and fix fusion.
--         We should be able to eliminate a lot of the indexing happening in the 
--         inner loop by being cleverer about the loop state.
streamSegsFromUSSegd
        :: Unbox a
        => USSegd               -- ^ Segment descriptor defining segments based on source vectors.
        -> V.Vector (Vector a)  -- ^ Source vectors.
        -> S.Stream a
        
streamSegsFromUSSegd ussegd@(USSegd starts sources usegd) pdatas
 = let  
        -- length of each segment
        pseglens        = lengthsUSegd usegd
 
        -- We've finished streaming this pseg
        {-# INLINE fn #-}
        fn (pseg, ix)
         -- All psegs are done.
         | pseg >= lengthUSSegd ussegd
         = return $ S.Done
         
         -- Current pseg is done
         | ix   >= pseglens U.! pseg 
         = fn (pseg + 1, 0)

         -- Stream an element from this pseg
         | otherwise
         = let  srcid   = sources    U.! pseg
                pdata   = pdatas     V.!  srcid
                start   = starts     U.! pseg
                result  = pdata U.! (start + ix)
           in   return $ S.Yield result 
                                (pseg, ix + 1)

   in   M.Stream fn (0, 0) S.Unknown


