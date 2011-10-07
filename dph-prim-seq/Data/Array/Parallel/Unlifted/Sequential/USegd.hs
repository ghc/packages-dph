{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
#include "fusion-phases.h"

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
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
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

mkUSegd = USegd
{-# INLINE mkUSegd #-}


-- | O(1). Check the internal consistency of a scattered segment descriptor.
--   As the indices and elemens field can be generated based on the segment
--   lengths, we check the consistency by rebuilding these fields and 
--   comparing the rebuilt ones against the originals.
valid :: USegd -> Bool
valid usegd@(USegd lengths _ _)
        = usegd == fromLengths lengths
{-# NOINLINE valid #-}
--  NOINLINE because it's only enabled during debugging anyway.


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: USegd
empty   = USegd V.empty V.empty 0
{-# INLINE_U empty #-}


-- | O(1). Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> USegd
singleton n
        = USegd (V.singleton n) (V.singleton 0) n
{-# INLINE_U singleton #-}


-- | O(n). Convert a length array into a segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that. Runtime is O(n) in the number of segments.
--
fromLengths :: Vector Int -> USegd
fromLengths lens
        = USegd lens (V.scanl (+) 0 lens) (V.sum lens)
{-# INLINE_U fromLengths #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: USegd -> Int
length          = V.length . usegd_lengths
{-# INLINE length #-}


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: USegd -> Vector Int
takeLengths     = usegd_lengths
{-# INLINE takeLengths #-}


-- | O(1). Yield the segment indices of a segment descriptor.
takeIndices :: USegd -> Vector Int
takeIndices     = usegd_indices
{-# INLINE takeIndices #-}


-- | O(1). Yield the number of data elements.
takeElements :: USegd -> Int
takeElements    = usegd_elements
{-# INLINE takeElements #-}


-- | O(1). Get the length and segment index of a segment
getSeg :: USegd -> Int -> (Int, Int)
getSeg (USegd lengths indices _ ) ix
 =      ( lengths V.! ix
        , indices V.! ix)
{-# INLINE_U getSeg #-}


-- Operators ------------------------------------------------------------------
-- | O(segs). Produce a segment descriptor that describes the result of appending 
--   two arrays.
append :: USegd -> USegd -> USegd
append (USegd lengths1 indices1 elems1)
            (USegd lengths2 indices2 elems2)
 = USegd (lengths1 V.++ lengths2)
         (indices1 V.++ V.map (+ elems1) indices2)
         (elems1 + elems2)
{-# INLINE_U append #-}


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
slice segd i n
        = fromLengths $ V.slice (takeLengths segd) i n
{-# INLINE_U slice #-}


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
extract segd i n 
        = fromLengths $ V.extract (takeLengths segd) i n
{-# INLINE_U extract #-}

