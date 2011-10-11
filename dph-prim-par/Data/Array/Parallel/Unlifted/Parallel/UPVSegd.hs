{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- | Parallel virtual segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPVSegd (
        -- * Types
        UPVSegd,

        -- * Consistency check
        valid,
        
        -- * Constructors
        mkUPVSegd,
        fromUPSegd,
        fromUPSSegd,
        empty,
        singleton,
        
        -- * Predicates
        isManifest,
        isContiguous,
        
        -- * Projections
        length,
        takeVSegids,
        takeUPSSegd,
        takeLengths,
        getSeg,

        -- * Demotion
        demoteToUPSSegd,
        unsafeDemoteToUPSegd,

        -- * Operators
        updateVSegs,
        appendWith,
        combine2,
) where
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.UPSel              (UPSel2)
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd            (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd             (UPSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Pretty                               hiding (empty)
import Prelude                                                  hiding (length)

import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSel    as UPSel
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd  as UPSSegd


-- UPVSegd ---------------------------------------------------------------------
-- | A parallel virtual segment descriptor is an extension of `UPSSegd`
--   that explicitly represents sharing of data between multiple segments.
--   
--   TODO: It would probably be better to represent the vsegids as a lens (function)
--         instead of a vector of segids. Much of the time the vsegids are just [0..n] 
--
data UPVSegd 
        = UPVSegd 
        { upvsegd_manifest      :: !Bool
          -- ^ When the vsegids field holds a lazy (V.enumFromTo 0 (len - 1))
          --   then this field is True. This lets us perform some operations like
          --   demoteToUPSSegd without actually creating it.
        
        , upvsegd_vsegids       :: Vector Int
          -- ^ Virtual segment identifiers that indicate what physical segment
          --   to use for each virtual segment. 
          --
          --   IMPORTANT:
          ---   This field must be lazy (no bang) because when it has the value
          --    (V.enumFromTo 0 (len - 1)) we want to avoid building the enumeration
          --    unless it's strictly demanded.
          
        , upvsegd_upssegd       :: !UPSSegd }
        deriving (Show)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical UPVSegd where
 pprp (UPVSegd _ vsegids upssegd)
  = vcat
  [ text "UPVSegd" $$ (nest 7 $ text "vsegids: " <+> (text $ show $ V.toList vsegids))
  , pprp upssegd ]


-- | O(1). Check the internal consistency of a virutal segmentation descriptor.
--
--   * TODO: this doesn't do any checks yet.
--
valid :: UPVSegd -> Bool
valid UPVSegd{} = True
{-# NOINLINE valid #-}
--  NOINLINE because it's only used during debugging anyway.


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new virtual segment descriptor.
mkUPVSegd
        :: Vector Int   -- ^ Array saying which physical segment to use for each virtual segment.
        -> UPSSegd      -- ^ Scattered segment descriptor defining the physical segments.
        -> UPVSegd

mkUPVSegd = UPVSegd False
{-# NOINLINE mkUPVSegd #-}


-- | O(segs). Promote a `UPSSegd` to a `UPVSegd`.
--   The result contains one virtual segment for every physical segment
--   defined by the `UPSSegd`.
--
--   TODO: make this parallel, use parallel version of enumFromTo.
--
fromUPSSegd :: UPSSegd -> UPVSegd
fromUPSSegd upssegd
    = UPVSegd   True
                (V.enumFromTo 0 (UPSSegd.length upssegd - 1))
                upssegd
{-# NOINLINE fromUPSSegd #-}


-- | O(segs). Promote a `UPSegd` to a `UPVSegd`.
--   All segments are assumed to come from a flat array with sourceid 0.
--   The result contains one virtual segment for every physical segment
--   the provided `UPSegd`.
--
fromUPSegd :: UPSegd -> UPVSegd
fromUPSegd      = fromUPSSegd . UPSSegd.fromUPSegd
{-# NOINLINE fromUPSegd #-}


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: UPVSegd
empty           = UPVSegd True V.empty UPSSegd.empty
{-# NOINLINE empty #-}


-- | O(1). Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singleton :: Int -> UPVSegd
singleton n     = UPVSegd True (V.singleton 0) (UPSSegd.singleton n)
{-# NOINLINE singleton #-}


-- Predicates -----------------------------------------------------------------
-- | O(1). Checks whether all the segments are manifest (unshared / non-virtual).
--   If this is the case, then the vsegids field will be [0..len-1]. 
--
--   Consumers can check this field, avoid demanding the vsegids field.
--   This can avoid the need for it to be generated in the first place, due to
--   lazy evaluation.
--
isManifest :: UPVSegd -> Bool
isManifest      = upvsegd_manifest
{-# INLINE isManifest #-}


-- | O(1). True when the starts are identical to the usegd indices field and
--   the sources are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
--
isContiguous :: UPVSegd -> Bool
isContiguous    = UPSSegd.isContiguous . upvsegd_upssegd
{-# INLINE isContiguous #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Yield the overall number of segments.
length :: UPVSegd -> Int
length          = V.length . upvsegd_vsegids
{-# INLINE length #-}


-- | O(1). Yield the virtual segment ids of `UPVSegd`.
takeVSegids :: UPVSegd -> Vector Int
takeVSegids     = upvsegd_vsegids
{-# INLINE takeVSegids #-}


-- | O(1). Yield the `UPSSegd` of `UPVSegd`.
takeUPSSegd :: UPVSegd -> UPSSegd
takeUPSSegd     = upvsegd_upssegd
{-# INLINE takeUPSSegd #-}


-- | O(segs). Yield the lengths of the segments described by a `UPVSegd`.
takeLengths :: UPVSegd -> Vector Int
takeLengths (UPVSegd manifest vsegids upssegd)
 | manifest     = UPSSegd.takeLengths upssegd
 | otherwise    = V.map (UPSSegd.takeLengths upssegd V.!) vsegids
{-# NOINLINE takeLengths #-}


-- | O(1). Get the length, starting index, and source id of a segment.
--
--  NOTE: We don't return the segment index field from the USSegd as this refers
--        to the flat index relative to the SSegd array, rather than 
--        relative to the UVSegd array. If we tried to promote the USSegd index
--        to a UVSegd index it could overflow.
--
getSeg :: UPVSegd -> Int -> (Int, Int, Int)
getSeg (UPVSegd _ vsegids upssegd) ix
 = let  (len, _index, start, source) = UPSSegd.getSeg upssegd (vsegids V.! ix)
   in   (len, start, source)
{-# INLINE_UP getSeg #-}


-- Demotion -------------------------------------------------------------------
-- | O(segs). Yield a `UPSSegd` that describes each segment of a `UPVSegd`
--   individually.
--
--   * By doing this we lose information about virtual segments corresponding
--     to the same physical segments.
-- 
--   * This operation is used in concatPR as the first step in eliminating
--     segmentation from a nested array.
-- 
demoteToUPSSegd :: UPVSegd -> UPSSegd
demoteToUPSSegd (UPVSegd True _vsegids upssegd)
 = upssegd

demoteToUPSSegd (UPVSegd False vsegids upssegd)
 = let  starts'         = bpermuteUP (UPSSegd.takeStarts  upssegd) vsegids
        sources'        = bpermuteUP (UPSSegd.takeSources upssegd) vsegids
        lengths'        = bpermuteUP (UPSSegd.takeLengths upssegd) vsegids
        upsegd'         = UPSegd.fromLengths lengths'
   in   UPSSegd.mkUPSSegd starts' sources' upsegd'
{-# NOINLINE demoteToUPSSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.
--  In core we want to see when VSegds are being demoted.


-- | O(segs). Given an virtual segment descriptor, produce a `UPSegd` that
--   that describes the entire array.
--
--   WARNING:
--   Trying to take the `UPSegd` of a nested array that has been constructed with
--   replication can cause index overflow. This is because the virtual size of
--   the corresponding flat data can be larger than physical memory.
-- 
--   You should only apply this function to a nested array when you're about
--   about to construct something with the same size as the corresponding
--   flat array. In this case the index overflow doesn't matter too much
--   because the program would OOM anyway.
--
unsafeDemoteToUPSegd :: UPVSegd -> UPSegd
unsafeDemoteToUPSegd (UPVSegd _ vsegids upssegd)
        = UPSegd.fromLengths
        $ bpermuteUP (UPSSegd.takeLengths upssegd) vsegids
{-# NOINLINE unsafeDemoteToUPSegd #-}
--  NOINLINE because it's complicated and won't fuse with anything.
--  In core we want to see when VSegds are being demoted.


-- Operators ------------------------------------------------------------------
-- | Update the virtual segment ids of a UPVSegd and force out unreachable
--   physical segments from the contained UPSSegd.
--
--   * TODO: make this parallel. It runs the sequential 'cull' then reconstructs
--     the UPSSegd.
-- 
updateVSegs :: (Vector Int -> Vector Int) -> UPVSegd -> UPVSegd
updateVSegs fUpdate (UPVSegd _ vsegids upssegd)
 = let  (vsegids', ussegd') 
                = USSegd.cullOnVSegids (fUpdate vsegids) 
                $ UPSSegd.takeUSSegd upssegd

   in   UPVSegd False vsegids' (UPSSegd.fromUSSegd ussegd')
{-# INLINE_UP updateVSegs #-}
--  INLINE_UP because we want to inline the parameter function fUpdate.


-- Append ---------------------------------------------------------------------
-- | Produce a segment descriptor that describes the result of appending two arrays.
-- 
--   * TODO: make this parallel.
--
appendWith
        :: UPVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UPVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UPVSegd

appendWith
        (UPVSegd _ vsegids1 upssegd1) pdatas1
        (UPVSegd _ vsegids2 upssegd2) pdatas2

 = let  -- vsegids releative to appended psegs
        vsegids1' = vsegids1
        vsegids2' = V.map (+ UPSSegd.length upssegd1) vsegids2
        
        -- append the vsegids
        vsegids'  = vsegids1' V.++ vsegids2'

        -- All data from the source arrays goes into the result
        upssegd'  = UPSSegd.appendWith
                                upssegd1 pdatas1
                                upssegd2 pdatas2
                                 
   in   UPVSegd False vsegids' upssegd'
{-# NOINLINE appendWith #-}


-- Combine --------------------------------------------------------------------
-- | Combine two virtual segment descriptors.
--
--   * TODO: make this parallel. 
--
combine2
        :: UPSel2
        -> UPVSegd -> Int   -- ^ uvsegd of array, and number of physical data arrays
        -> UPVSegd -> Int   -- ^ uvsegd of array, and number of physical data arrays
        -> UPVSegd
        
combine2
        upsel2
        (UPVSegd _ vsegids1 upssegd1) pdatas1
        (UPVSegd _ vsegids2 upssegd2) pdatas2

 = let  -- vsegids relative to combined psegs
        vsegids1' = vsegids1
        vsegids2' = V.map (+ (V.length vsegids1)) vsegids2

        -- combine the vsegids
        vsegids'  = V.combine2ByTag (UPSel.tagsUPSel2 upsel2)
                                    vsegids1' vsegids2'

         -- All data from the source arrays goes into the result
        upssegd'  = UPSSegd.appendWith
                                upssegd1 pdatas1
                                upssegd2 pdatas2
                                  
   in   UPVSegd False vsegids' upssegd'
{-# NOINLINE combine2 #-}

