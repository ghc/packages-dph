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
        
        -- * Predicates
        isManifest,
        isContiguous,
        
        -- * Projections
        length,
        takeVSegids,
        takeUSSegd,
        takeLengths,
        getSeg,

        -- * Operators
        appendWith,
        combine2,
        updateVSegs,
        updateVSegsReachable,
        demoteToUSSegd,
        unsafeDemoteToUSegd)
where
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as U
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd

here :: String -> String 
here s = "Data.Array.Parallel.Unlifted.Sequential.UVSegd." ++ s


-- UVSegd ---------------------------------------------------------------------
-- | Virtual segment descriptors. 
--   Represents an index space transformation between indices for the nested
--   array and indices for the physical data.
--   
--   * TODO: It would probably be better to represent the vsegids as a lens (function)
--           instead of a vector of segids. Much of the time the vsegids are just @[0..n]@
--
data UVSegd 
        = UVSegd 
        { uvsegd_manifest       :: !Bool
          -- ^ When the vsegids field holds a lazy @(U.enumFromTo 0 (len - 1))@
          --   then this field is True. This lets us perform some operations like
          --   `demoteToUPSSegd` without actually creating it.
          
        , uvsegd_vsegids        :: (Vector Int) 
          -- ^ Array saying which physical segment to use for each virtual segment. 

        , uvsegd_ussegd         :: !USSegd
          -- ^ Slice segment descriptor describing physical segments.
        }
        deriving (Show)


instance PprPhysical UVSegd where
 pprp (UVSegd _ vsegids ussegd)
  = vcat
  [ text "UVSegd" $$ (nest 7 $ text "vsegids: " <+> (text $ show $ U.toList vsegids))
  , pprp ussegd ]



-- | O(1). Check the internal consistency of a virutal segmentation descriptor.
--
--   * TODO: check that all vsegs point to a valid pseg
valid :: UVSegd -> Bool
valid (UVSegd _ vsegids ussegd)
        = U.length vsegids == USSegd.length ussegd
{-# NOINLINE valid #-}
--  NOINLINE because it's only enabled during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new virtual segment descriptor.
--   All the provided arrays must have the same lengths.
mkUVSegd
        :: Vector Int   -- ^ Array saying which physical segment to use for each
                        --   virtual segment.
        -> USSegd       -- ^ Slice segment descriptor describing physical segments.
        -> UVSegd

mkUVSegd = UVSegd False
{-# INLINE mkUVSegd #-}


-- | O(segs). Promote a plain `USSegd` to a `UVSegd`
--   The result contains one virtual segment for every physical segment
--   the provided `USSegd`.
--
fromUSSegd :: USSegd -> UVSegd
fromUSSegd ussegd
        = UVSegd True
                 (U.enumFromTo 0 (USSegd.length ussegd - 1))
                 ussegd
{-# INLINE_U fromUSSegd #-}


-- | O(segs). Promote a plain `USegd` to a `UVSegd`.
--   All segments are assumed to come from a flat array with sourceid 0.
--   The result contains one virtual segment for every physical segment
--   the provided USegd.
--
fromUSegd :: USegd -> UVSegd
fromUSegd
        = fromUSSegd . USSegd.fromUSegd
{-# INLINE_U fromUSegd #-}


-- | O(1). Construct an empty segment descriptor, with no elements or segments.
empty :: UVSegd
empty   = UVSegd True U.empty USSegd.empty
{-# INLINE_U empty #-}


-- | O(1). Construct a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singleton :: Int -> UVSegd
singleton n 
        = UVSegd True (U.singleton 0) (USSegd.singleton n)
{-# INLINE_U singleton #-}


-- Predicates -----------------------------------------------------------------
-- | O(1). Checks whether all the segments are manifest (unshared / non-virtual).
--   If this is the case, then the vsegids field will be [0..len-1]. 
--
--   Consumers can check this field, avoid demanding the vsegids field.
--   This can avoid the need for it to be generated in the first place, due to
--   lazy evaluation.
--
isManifest :: UVSegd -> Bool
isManifest      = uvsegd_manifest
{-# INLINE isManifest #-}


-- | O(1). Checks whether the starts are identical to the usegd indices field and
--   the sourceids are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
--
isContiguous :: UVSegd -> Bool
isContiguous    = USSegd.isContiguous . uvsegd_ussegd
{-# INLINE isContiguous #-}


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
length          = U.length . uvsegd_vsegids
{-# INLINE length #-}


-- | O(segs). Yield the lengths of the segments described by a `UVSegd`.
takeLengths :: UVSegd -> Vector Int
takeLengths (UVSegd _ vsegids ussegd)
        = U.map (U.index (here "takeLengths") (USSegd.takeLengths ussegd)) vsegids
{-# INLINE_U takeLengths #-}


-- | O(1). Get the length, starting index, and source id of a segment.

--  NOTE: We don't return the segment index field from the USSegd as this refers
--        to the flat index relative to the SSegd array, rather than 
--        relative to the UVSegd array. If we tried to promote the USSegd index
--        to a UVSegd index it could overflow.
--
getSeg :: UVSegd -> Int -> (Int, Int, Int)
getSeg (UVSegd _ vsegids ussegd) ix
 = let  (len, _index, start, source) 
                = USSegd.getSeg ussegd (U.index (here "getSeg") vsegids ix)
   in   (len, start, source)
{-# INLINE_U getSeg #-}

   
-- Operators ------------------------------------------------------------------
-- | Update the vsegids of `UPVSegd`, and then cull the physical
--   segment descriptor so that all phsyical segments are reachable from
--   some virtual segment.
--
--   This function lets you perform filtering operations on the virtual segments,
--   while maintaining the invariant that all physical segments are referenced
--   by some virtual segment.
-- 
updateVSegs :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegs f (UVSegd _ vsegids ussegd)
 = let  (vsegids', ussegd') = USSegd.cullOnVSegids (f vsegids) ussegd
   in   UVSegd False vsegids' ussegd'
{-# INLINE_U updateVSegs #-}
--  INLINE_UP because we want to inline the parameter function fUpdate.


-- | Update the vsegids of `UPVSegd`, where the result covers
--   all physical segments.
--
--   * The resulting vsegids must cover all physical segments.
--     If they do not then there will be physical segments that are not 
--     reachable from some virtual segment, and performing operations like
--     segmented fold will waste work.
--
--   * Using this version saves performing the 'cull' operation which 
--     discards unreachable physical segments. This is O(result segments), 
--     but can be expensive in absolute terms.
--   
updateVSegsReachable :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
updateVSegsReachable fUpdate (UVSegd _ vsegids upssegd)
 = UVSegd False (fUpdate vsegids) upssegd
{-# INLINE_UP updateVSegsReachable #-}
--  INLINE_UP because we want to inline the parameter function fUpdate.


-- | O(segs). Yield a `USSegd` that describes each segment of a `UVSegd` individually.
-- 
--   * By doing this we lose information about virtual segments corresponding
--     to the same physical segments.
-- 
--   * This operation is used in concatPR as the first step in eliminating
--     segmentation from a nested array.
-- 
demoteToUSSegd :: UVSegd -> USSegd
demoteToUSSegd (UVSegd _ vsegids ussegd)
 = let  starts'         = U.bpermute (USSegd.takeStarts  ussegd)  vsegids
        sources'        = U.bpermute (USSegd.takeSources ussegd) vsegids
        lengths'        = U.bpermute (USSegd.takeLengths ussegd) vsegids
        usegd'          = USegd.fromLengths lengths'
   in   USSegd.mkUSSegd starts' sources' usegd'
{-# NOINLINE demoteToUSSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.


-- | O(segs). Yield a `USegd` that describes each segment of a `UVSegd`
--   individually, assuming all segments have been concatenated to 
--   remove scattering.
--
--   /WARNING/: Trying to take the `UPSegd` of a nested array that has been
--   constructed with replication can cause index space overflow. This is
--   because the virtual size of the corresponding flat data can be larger
--   than physical memory. If this happens then indices fields and 
--   element count in the result will be invalid.
-- 
--
unsafeDemoteToUSegd :: UVSegd -> USegd
unsafeDemoteToUSegd (UVSegd _ vsegids ussegd)
        = USegd.fromLengths
        $ U.bpermute (USSegd.takeLengths ussegd) vsegids
{-# NOINLINE unsafeDemoteToUSegd #-}
--  NOINLINE because it won't fuse with anything.


-- append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor describing the result of appending two arrays.

--   Note that the implementation of this is similar to `combine2UVSegd`
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
appendWith
        :: UVSegd       -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> UVSegd       -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> UVSegd

appendWith
        (UVSegd _ vsegids1 ussegd1) pdatas1
        (UVSegd _ vsegids2 ussegd2) pdatas2

 = let  -- vsegids releative to appended psegs
        vsegids1' = vsegids1
        vsegids2' = U.map (+ USSegd.length ussegd1) vsegids2
        
        -- append the vsegids
        vsegids'  = vsegids1' U.++ vsegids2'

        -- All data from the source arrays goes into the result
        ussegd'   = USSegd.appendWith
                                ussegd1 pdatas1
                                ussegd2 pdatas2
                                 
   in   UVSegd False vsegids' ussegd'
{-# INLINE_U appendWith #-}


-- combine --------------------------------------------------------------------
-- | O(n). Combine two virtual segment descriptors.


-- Note that the implementation of this is similar to `appendUVSegd`
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
        :: USel2       -- ^ Selector for the combine operation.
        -> UVSegd      -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> UVSegd      -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> UVSegd
        
combine2  usel2
        (UVSegd _ vsegids1 ussegd1) pdatas1
        (UVSegd _ vsegids2 ussegd2) pdatas2

 = let  -- vsegids relative to combined psegs
        vsegids1' = vsegids1
        vsegids2' = U.map (+ (U.length vsegids1)) vsegids2

        -- combine the vsegids
        vsegids'  = U.combine2ByTag (tagsUSel2 usel2)
                                    vsegids1' vsegids2'

         -- All data from the source arrays goes into the result
        ussegd'   = USSegd.appendWith
                                ussegd1 pdatas1
                                ussegd2 pdatas2
                                  
   in   UVSegd False vsegids' ussegd'
{-# INLINE_U combine2 #-}
