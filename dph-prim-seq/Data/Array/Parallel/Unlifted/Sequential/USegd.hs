{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
-- | Segment Descriptors
module Data.Array.Parallel.Unlifted.Sequential.USegd (
  -- * Types
  USegd,

  -- * Constructors
  mkUSegd, valid,
  empty, singleton,
  fromLengths,

  -- * Projections
  length,
  takeLengths, takeIndices, takeElements, 
  getSeg,

  -- * Operations
  append, slice, extract
) where
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector, MVector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)


-- | Segment descriptors represent an index space transform between the indicies
--   of a nested array and the indices of a flat array. It stores the lengths and
--   starting indices of each segment in the flat array.
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

{-# INLINE_U mkUSegd #-}
mkUSegd = USegd


-- | O(1). Check the internal consistency of a scattered segment descriptor.
--   As the indices and elemens field can be generated based on the segment
--   lengths, we check the consistency by rebuilding these fields and 
--   comparing the rebuilt ones against the originals.
valid :: USegd -> Bool
{-# INLINE_U valid #-}
valid usegd@(USegd lengths _ _)
        = usegd == fromLengths lengths


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: USegd
{-# INLINE_U empty #-}
empty = USegd V.empty V.empty 0


-- | O(1). Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> USegd
{-# INLINE_U singleton #-}
singleton n = USegd (V.singleton n) (V.singleton 0) n


-- | O(n). Convert a length array into a segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that. Runtime is O(n) in the number of segments.
--
fromLengths :: Vector Int -> USegd
{-# INLINE_U fromLengths #-}
fromLengths lens
        = USegd lens (V.scanl (+) 0 lens) (V.sum lens)


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
length :: USegd -> Int
{-# INLINE_U length #-}
length = V.length . usegd_lengths


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: USegd -> Vector Int
{-# INLINE_U takeLengths #-}
takeLengths = usegd_lengths


-- | O(1). Yield the segment indices of a segment descriptor.
takeIndices :: USegd -> Vector Int
{-# INLINE_U takeIndices #-}
takeIndices = usegd_indices


-- | O(1). Yield the number of data elements.
takeElements :: USegd -> Int
{-# INLINE_U takeElements #-}
takeElements = usegd_elements


-- | O(1). Get the length and segment index of a segment
getSeg :: USegd -> Int -> (Int, Int)
{-# INLINE_U getSeg #-}
getSeg (USegd lengths indices _ ) ix
 =      ( lengths V.! ix
        , indices V.! ix)


-- Operators ------------------------------------------------------------------
-- | O(segs). Produce a segment descriptor that describes the result of appending 
--   two arrays.
append :: USegd -> USegd -> USegd
{-# INLINE_U append #-}
append (USegd lengths1 indices1 elems1)
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
slice
        :: USegd        -- ^ source segment descriptor
        -> Int          -- ^ index of first segment
        -> Int          -- ^ number of segments to slice out
        -> USegd
        
{-# INLINE_U slice #-}
slice segd i n
        = fromLengths $ V.slice (takeLengths segd) i n


-- | Extract a slice of a segment descriptor, copying everything.
--
--   In contrast to `sliceUSegd`, this function copies the array of 
--   segment lengths as well as recomputing the starting indices of each.
--
--   NOTE: In the new segment descriptor, the starting index of the first
--         segment will be 0.
extract
        :: USegd        -- ^ source segment desciptor
        -> Int          -- ^ index of the first segment
        -> Int          -- ^ number of segments to extract out
        -> USegd

{-# INLINE_U extract #-}
extract segd i n 
        = fromLengths $ V.extract (takeLengths segd) i n

