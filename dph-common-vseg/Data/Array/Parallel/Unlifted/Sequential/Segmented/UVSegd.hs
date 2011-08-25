
-- | Segment descriptors for virtual arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.UVSegd (
        -- * Types
        UVSegd(..),

        -- * Consistency check
        validUVSegd,
        
        -- * Constructors
        mkUVSegd,
        emptyUVSegd,
        singletonUVSegd,
        promoteUSegdToUVSegd,
        
        -- * Projections
        lengthUVSegd,
        lengthsUVSegd,
        getSegOfUVSegd,

        -- * Operators
        appendUVSegd,
        updateVSegsOfUVSegd,
        demoteUVSegdToUSSegd,
        unsafeMaterializeUVSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USSegd
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector as V


-- UVSegd ---------------------------------------------------------------------
-- TODO shift this to its own module
-- | Defines a virutal nested array based on physical segmentation.
--
--   Or alternatively: represents an index space transformation between
--   indices for the nested array and indices for the physical data.
--   
--   TODO: It'd probably be better to represent the vsegids as a lens instead
--         of a vector of segids. Much of the time the vsegids are just [0..n] 
--
data UVSegd 
        = UVSegd 
        { uvsegd_vsegids :: !(Vector Int) 
          -- ^ array saying which physical segment to use for each virtual segment 

        , uvsegd_ussegd  :: !USSegd
          -- ^ slice segment descriptor describing physical segments.
        }
        deriving (Show)


-- Constructors ---------------------------------------------------------------
-- | O(1). 
--   Construct a new slice segment descriptor.
--   All the provided arrays must have the same lengths.
mkUVSegd
        :: Vector Int   -- ^ array saying which physical segment to use for each
                        --   virtual segment.
        -> USSegd       -- ^ slice segment descriptor describing physical segments.
        -> UVSegd

{-# INLINE mkUVSegd #-}
mkUVSegd = UVSegd


-- | O(1).
--   Check the internal consistency of a virutal segmentation descriptor.
--   TODO: check that all vsegs point to a valid pseg
validUVSegd :: UVSegd -> Bool
{-# INLINE validUVSegd #-}
validUVSegd (UVSegd vsegids ussegd)
        = V.length vsegids == lengthUSSegd ussegd


-- | O(1).
--  Yield an empty segment descriptor, with no elements or segments.
emptyUVSegd :: UVSegd
{-# INLINE emptyUVSegd #-}
emptyUVSegd = UVSegd V.empty emptyUSSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singletonUVSegd :: Int -> UVSegd
{-# INLINE singletonUVSegd #-}
singletonUVSegd n 
        = UVSegd (V.singleton 0) (singletonUSSegd n)


-- | O(segs). 
--   Promote a plain USegd to a UVSegd
--   All segments are assumed to come from a flat array with sourceid 0.
--   The result contains one virtual segment for every physical segment
--   the provided USegd.
--
promoteUSegdToUVSegd :: USegd -> UVSegd
{-# INLINE promoteUSegdToUVSegd #-}
promoteUSegdToUVSegd usegd
        = UVSegd (V.enumFromTo 0 (lengthUSegd usegd - 1))
                 (promoteUSegdToUSSegd usegd)


-- Projections ----------------------------------------------------------------
lengthUVSegd :: UVSegd -> Int
lengthUVSegd (UVSegd vsegids _)
        = V.length vsegids

-- | O(segs).
--   Yield the lengths of the segments described by a `UVSegd`.
lengthsUVSegd :: UVSegd -> Vector Int
{-# INLINE lengthsUVSegd #-}
lengthsUVSegd (UVSegd vsegids ussegd)
        = V.map ((ussegd_lengths ussegd) V.!) vsegids


-- | O(1).
--  Get the length, starting index, and sourceid of a segment.
getSegOfUVSegd :: Int -> UVSegd -> (Int, Int, Int)
{-# INLINE getSegOfUVSegd #-}
getSegOfUVSegd ix (UVSegd vsegids ussegd)
        = getSegOfUSSegd (vsegids V.! ix) ussegd
        

-- Operators ------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor describing the result of appending
--   two arrays.
appendUVSegd
        :: UVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd

appendUVSegd (UVSegd vsegids1 ussegd1) pdatas1
             (UVSegd vsegids2 ussegd2) pdatas2
        = UVSegd (vsegids1  V.++  V.map (+ (V.length vsegids1)) vsegids2)
                 (appendUSSegd ussegd1 pdatas1 ussegd2 pdatas2)


-- | TODO: automatically force out unreachable psegs here.
updateVSegsOfUVSegd :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegsOfUVSegd f (UVSegd vsegids ussegd)
        = UVSegd (f vsegids) ussegd


-- | O(segs)
--   Yield a `USSegd` that describes each segment of a `UVSegd` individually.
-- 
--   * By doing this we lose information about virtual segments corresponding
--     to the same physical segments.
-- 
--   * This operation is used in concatPR as the first step in eliminating
--     segmentation from a nested array.
-- 
demoteUVSegdToUSSegd :: UVSegd -> USSegd
demoteUVSegdToUSSegd (UVSegd vsegids ussegd)
        = mkUSSegd (V.bpermute (lengthsUSSegd ussegd) vsegids)
                   (V.bpermute (indicesUSSegd ussegd) vsegids)
                   (V.bpermute (sourcesUSSegd ussegd) vsegids)


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

