-- | Segment Descriptors
module Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (
  -- * Types
  USegd,

  -- * Constructors
  mkUSegd, validUSegd,
  emptyUSegd, singletonUSegd, lengthsToUSegd,

  -- * Projections
  lengthUSegd, lengthsUSegd, indicesUSegd, elementsUSegd, 
  getSegOfUSegd,

  -- * Operations
  appendUSegd, sliceUSegd, extractUSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Pretty

-- | Segment descriptors represent the structure of nested arrays.
--  For each segment, it stores the length and the starting index in the flat data array.
--
--   Example:
--
--   @
--    flat array data: [1 2 3 4 5 6 7 8]
--      (segmentation)  --- ----- - ---
--      segd  lengths: [2, 3, 1, 2]
--            indices: [0, 2, 5, 6]
--           elements: 8 
--   @
data USegd 
        = USegd 
        { usegd_lengths  :: !(Vector Int)  -- ^ length of each segment
        , usegd_indices  :: !(Vector Int)  -- ^ index of each segment in the flat array
        , usegd_elements :: !Int           -- ^ total number of elements in the flat array
        } deriving (Show, Eq)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical USegd where
 pprp (USegd lengths indices elements)
  =   text "USegd" 
  $$  (nest 7 $ vcat
        [ text "lengths: " <+> (text $ show $ V.toList lengths)
        , text "indices: " <+> (text $ show $ V.toList indices)
        , text "elements:" <+> (text $ show elements)])


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUSegd 
        :: Vector Int   -- ^ length of each segment
        -> Vector Int   -- ^ starting index of each segment
        -> Int          -- ^ total number of elements in the flat array
        -> USegd

{-# INLINE mkUSegd #-}
mkUSegd = USegd


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
--   As the indices and elemens field can be generated based on the segment
--   lengths, we check the consistency by rebuilding these fields and 
--   comparing the rebuilt ones against the originals.
validUSegd :: USegd -> Bool
{-# INLINE validUSegd #-}
validUSegd usegd@(USegd lengths indices elems)
        = usegd == lengthsToUSegd lengths


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


-- | O(1). Get the length and segment index of a segment
getSegOfUSegd :: USegd -> Int -> (Int, Int)
getSegOfUSegd (USegd lengths indices _ ) ix
 =      ( lengths V.! ix
        , indices V.! ix)


-- Operators ------------------------------------------------------------------
-- | O(segs)
--   Produce a segment descriptor that describes the result of appending 
--   two arrays.
appendUSegd :: USegd -> USegd -> USegd
appendUSegd (USegd lengths1 indices1 elems1)
            (USegd lengths2 indices2 elems2)
 = USegd (lengths1 V.++ lengths2)
         (indices1 V.++ V.map (+ elems1) indices2)
         (elems1 + elems2)


-- | Extract a slice of a segment descriptor, avoiding copying where possible.
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


-- | Extract a slice of a segment descriptor, copying everything.
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

