{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
#include "fusion-phases.h"

-- | Segment descriptors for virtual arrays.
module Data.Array.Parallel.Unlifted.Sequential.UVSegd (
        -- * Types
        UVSegd(..),

        -- * Consistency check
        valid,
        
        -- * Constructors
        mkUVSegd,
        fromUSegd,
        fromUSSegd,
        empty,
        singleton,
        
        -- * Projections
        length,
        takeVSegids,
        takeUSSegd,
        takeLengths,
        getSeg,

        -- * Operators
        append,
        combine2,
        updateVSegs,
        toUSSegd,
        unsafeMaterialize
) where
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)

import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd


-- UVSegd ---------------------------------------------------------------------
-- | Virtual segment descriptors. 
--   Represents an index space transformation between indices for the nested
--   array and indices for the physical data.
--   
--   TODO: It would probably be better to represent the vsegids as a lens (function)
--         instead of a vector of segids. Much of the time the vsegids are just [0..n] 
--
data UVSegd 
        = UVSegd 
        { uvsegd_vsegids :: !(Vector Int) 
          -- ^ array saying which physical segment to use for each virtual segment 

        , uvsegd_ussegd  :: !USSegd
          -- ^ slice segment descriptor describing physical segments.
        }
        deriving (Show)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical UVSegd where
 pprp (UVSegd vsegids ussegd)
  = vcat
  [ text "UVSegd" $$ (nest 7 $ text "vsegids: " <+> (text $ show $ V.toList vsegids))
  , pprp ussegd ]



-- | O(1).
--   Check the internal consistency of a virutal segmentation descriptor.
--   TODO: check that all vsegs point to a valid pseg
valid :: UVSegd -> Bool
valid (UVSegd vsegids ussegd)
        = V.length vsegids == USSegd.length ussegd
{-# NOINLINE valid #-}
--  NOINLINE because it's only enabled during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new virtual segment descriptor.
--   All the provided arrays must have the same lengths.
mkUVSegd
        :: Vector Int   -- ^ array saying which physical segment to use for each
                        --   virtual segment.
        -> USSegd       -- ^ slice segment descriptor describing physical segments.
        -> UVSegd

mkUVSegd = UVSegd
{-# INLINE mkUVSegd #-}


-- | O(segs). Promote a plain USSegd to a UVSegd
--   The result contains one virtual segment for every physical segment
--   the provided USSegd.
--
fromUSSegd :: USSegd -> UVSegd
fromUSSegd ussegd
        = UVSegd (V.enumFromTo 0 (USSegd.length ussegd - 1))
                 ussegd
{-# INLINE_U fromUSSegd #-}


-- | O(segs). Promote a plain USegd to a UVSegd
--   All segments are assumed to come from a flat array with sourceid 0.
--   The result contains one virtual segment for every physical segment
--   the provided USegd.
--
fromUSegd :: USegd -> UVSegd
fromUSegd
        = fromUSSegd . USSegd.fromUSegd
{-# INLINE_U fromUSegd #-}


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: UVSegd
empty   = UVSegd V.empty USSegd.empty
{-# INLINE_U empty #-}


-- | O(1). Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singleton :: Int -> UVSegd
singleton n 
        = UVSegd (V.singleton 0) (USSegd.singleton n)
{-# INLINE_U singleton #-}

        

-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the vsegids of a `UVSegd`
takeVSegids :: UVSegd -> Vector Int
takeVSegids     = uvsegd_vsegids
{-# INLINE takeVSegids #-}


-- | O(1). Yield the `USSegd` of a `UVSegd`.
takeUSSegd :: UVSegd -> USSegd
takeUSSegd      = uvsegd_ussegd
{-# INLINE takeUSSegd #-}

-- | O(1). Yield the overall number of segments described by a `UVSegd`.
length :: UVSegd -> Int
length          = V.length . uvsegd_vsegids
{-# INLINE length #-}


-- | O(segs). Yield the lengths of the segments described by a `UVSegd`.
takeLengths :: UVSegd -> Vector Int
takeLengths (UVSegd vsegids ussegd)
        = V.map (USSegd.takeLengths ussegd V.!) vsegids
{-# INLINE_U takeLengths #-}


-- | O(1). Get the length, starting index, and source id of a segment.
--
--  NOTE: We don't return the segment index field from the USSegd as this refers
--        to the flat index relative to the SSegd array, rather than 
--        relative to the UVSegd array. If we tried to promote the USSegd index
--        to a UVSegd index it could overflow.
--
getSeg :: UVSegd -> Int -> (Int, Int, Int)
getSeg (UVSegd vsegids ussegd) ix
 = let  (len, _index, start, source) = USSegd.getSeg ussegd (vsegids V.! ix)
   in   (len, start, source)
{-# INLINE_U getSeg #-}

   
-- Operators ------------------------------------------------------------------
-- | TODO: automatically force out unreachable psegs here.
updateVSegs :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegs f (UVSegd vsegids ussegd)
 = let  (vsegids', ussegd') = USSegd.cullOnVSegids (f vsegids) ussegd
   in   UVSegd vsegids' ussegd'
{-# INLINE_U updateVSegs #-}


-- | O(segs). Yield a `USSegd` that describes each segment of a `UVSegd` individually.
-- 
--   * By doing this we lose information about virtual segments corresponding
--     to the same physical segments.
-- 
--   * This operation is used in concatPR as the first step in eliminating
--     segmentation from a nested array.
-- 
toUSSegd :: UVSegd -> USSegd
toUSSegd (UVSegd vsegids ussegd)
 = let  starts'         = V.bpermute (USSegd.takeStarts  ussegd)  vsegids
        sources'        = V.bpermute (USSegd.takeSources ussegd) vsegids
        lengths'        = V.bpermute (USSegd.takeLengths ussegd) vsegids
        usegd'          = USegd.fromLengths lengths'
   in   USSegd.mkUSSegd starts' sources' usegd'
{-# NOINLINE toUSSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.


-- | O(segs). Given an virtual segment descriptor, produce a plain USegd that
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
unsafeMaterialize :: UVSegd -> USegd
unsafeMaterialize (UVSegd vsegids ussegd)
        = USegd.fromLengths
        $ V.bpermute (USSegd.takeLengths ussegd) vsegids
{-# NOINLINE unsafeMaterialize #-}
--  NOINLINE because it won't fuse with anything.


-- append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor describing the result of appending two arrays.
--   Note that the implementation of this is similar to `combine2UVSegd`
--
-- @
--  source1
--    VIRT1 [[0],[4,2],[5,6,7,8,9]]
--    PHYS1 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [1,2,5]
--                  psegstarts: [0,1,3]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [0,4,2,5,6,7,8,9]
--
--  source2
--    VIRT2 [[1,2,3],[8,6,3],[9,3]]
--    PHYS2 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [3,3,2]
--                  psegstarts: [0,3,6]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [1,2,3,8,6,3,9,3]
--
--   appended
--    VIRT  [[0],[4,2],[5,6,7,8,9],[1,2,3],[8,6,3],[9,3]]
--          UVSegd  vsegids:    [0,1,2,3,4,5]  -- shift second half
--          USSegd  pseglens:   [1,2,5,3,3,2]  -- appended
--                  psegstarts: [0,1,3,0,3,6]  -- appended
--                  psegsrcs:   [0,0,0,1,1,1]  -- shift second half
--          PData   PInt [0,4,2,5,6,7,8,9]     -- both pdatas in result
--                  PInt [1,2,3,8,6,3,9,3]     -- ...
-- @
-- 
append  :: UVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd

append  (UVSegd vsegids1 ussegd1) pdatas1
        (UVSegd vsegids2 ussegd2) pdatas2

 = let  -- vsegids releative to appended psegs
        vsegids1' = vsegids1
        vsegids2' = V.map (+ USSegd.length ussegd1) vsegids2
        
        -- append the vsegids
        vsegids'  = vsegids1' V.++ vsegids2'

        -- All data from the source arrays goes into the result
        ussegd'   = USSegd.append ussegd1 pdatas1
                                  ussegd2 pdatas2
                                 
   in   UVSegd vsegids' ussegd'
{-# INLINE_U append #-}


-- combine --------------------------------------------------------------------
-- | O(n)
--   Combine two virtual segment descriptors.
--   Note that the implementation of this is similar to `appendUVSegd`
--
-- @
-- source1
--    VIRT1 [[0],[4,2],[5,6,7,8,9]]
--    PHYS1 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [1,2,5]
--                  psegstarts: [0,1,3]
--                  psegsrcs:   [0,0,0]
--          PDATA   PInt [0,4,2,5,6,7,8,9]
--
-- source2
--    VIRT2 [[1,2,3],[8,6,3],[9,3]]
--    PHYS2 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [3,3,2]
--                  psegstarts: [0,3,6]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [1,2,3,8,6,3,9,3]
--
--   combined with tags [1,0,0,1,0,1]
--    VIRT  [[1,2,3],[0],[4,2],[8,6,3],[5,6,7,8,9],[9,3]]
--    PHYS  VSSegd  vsegids:    [3,0,1,4,2,5] -- combine shifted vsegs
--          USSegd  pseglens:   [1,2,5,3,3,2] -- appended
--                  psegstarts: [0,1,3,0,3,6] -- appended
--                  psegsrcs:   [0,0,0,1,1,1] -- shift second half
--          PData   PInt [0,4,2,5,6,7,8,9]    -- both pdatas in result
--                  PInt [1,2,3,8,6,3,9,3]
-- @  
-- 
combine2
        :: USel2
        -> UVSegd -> Int   -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd -> Int   -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd
        
combine2  usel2
        (UVSegd vsegids1 ussegd1) pdatas1
        (UVSegd vsegids2 ussegd2) pdatas2

 = let  -- vsegids relative to combined psegs
        vsegids1' = vsegids1
        vsegids2' = V.map (+ (V.length vsegids1)) vsegids2

        -- combine the vsegids
        vsegids'  = V.combine2ByTag (tagsUSel2 usel2)
                                    vsegids1' vsegids2'

         -- All data from the source arrays goes into the result
        ussegd'   = USSegd.append ussegd1 pdatas1
                                  ussegd2 pdatas2
                                  
   in   UVSegd vsegids' ussegd'
{-# INLINE_U combine2 #-}
