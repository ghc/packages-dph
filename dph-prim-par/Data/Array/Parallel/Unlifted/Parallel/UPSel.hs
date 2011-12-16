{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel selectors.
module Data.Array.Parallel.Unlifted.Parallel.UPSel (
  -- * Types
  UPSel2,
  UPSelRep2,

  -- * Operations
  tagsUPSel2,
  indicesUPSel2,
  elementsUPSel2_0,
  elementsUPSel2_1,
  selUPSel2,
  repUPSel2,
  mkUPSel2,
  mkUPSelRep2,
  indicesUPSelRep2,
  elementsUPSelRep2_0,
  elementsUPSelRep2_1,
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector   as US
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Base (Tag, tagToInt)


-- | Contains a selector `USel2`, as well as an `USelRep2` which says how 
--   to distribute this selector across the PEs. 
--
--   See @dph-prim-seq:Data.Array.Parallel.Unlifted.Sequential.Segmented.USel@
--   for more discussion of what selectors are for.
--
data UPSel2 
        = UPSel2 
        { upsel2_usel :: USel2
        , upsel2_rep  :: UPSelRep2 }


-- | A `UPSelRep2` describes how to distribute the two data vectors
--   corresponding to a `UPSel2` across several PEs.
--
--   Suppose we want to perform the following combine operation:
--
-- @
-- combine [0,0,1,1,0,1,0,0,1] [A0,A1,A2,A3,A4] [B0,B1,B2,B3] 
--   = [A0,A1,B0,B1,A2,B2,A3,A4,B3]
-- @
--
--   The first array is the tags array, that says which of the data arrays to
--   get each successive element from. As `combine` is difficult to compute
--   in parallel, if we are going to perform several combines with the same
--   tag array, we can precompute a selector that tells us where to get each
--   element. The selector contains the original tags, as well as the source
--   index telling us where to get each element for the result array.
-- 
-- @
-- tags:    [0,0,1,1,0,1,0,0,1]
-- indices: [0,1,0,1,2,2,3,4,3]
-- @
--
--  Suppose we want to distribute the combine operation across 3 PEs. It's
--  easy to split the selector like so:
--
-- @
--            PE0                PE1               PE2
-- tags:    [0,0,1]            [1,0,1]           [0,0,1] 
-- indices: [0,1,0]            [1,2,2]           [3,4,3]
-- @
--
--  We now need to split the two data arrays. Each PE needs slices of the data
--  arrays that correspond to the parts of the selector that were given to it.
--  For the current example we get:
--
-- @
--            PE0                PE1               PE2
-- tags:    [A0,A1]            [A2]              [A3,A4]
-- indices: [B0]               [B1,B2]           [B3]
-- @
--
--  The `UPSelRep2` contains the starting index and length of each of of these
--  slices:
--
-- @
--            PE0                PE1               PE2
--      ((0, 0), (2, 1))   ((2, 1), (1, 2))  ((3, 3), (2, 1))
--       indices   lens      indices  lens    indices  lens
-- @
--
type UPSelRep2
        = Dist ((Int,Int), (Int,Int))

-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Get the tags of a selector.
tagsUPSel2 :: UPSel2 -> Vector Tag
tagsUPSel2      = tagsUSel2 .  upsel2_usel
{-# INLINE tagsUPSel2 #-}


-- | O(1). Get the indices of a selector.
indicesUPSel2 :: UPSel2 -> Vector Int
indicesUPSel2   = indicesUSel2 . upsel2_usel
{-# INLINE indicesUPSel2 #-}


-- | O(1). Get the number of elements that will be taken from the first array.
elementsUPSel2_0 :: UPSel2 -> Int
elementsUPSel2_0 = elementsUSel2_0 . upsel2_usel
{-# INLINE elementsUPSel2_0 #-}


-- | O(1). Get the number of elements that will be taken from the second array.
elementsUPSel2_1 :: UPSel2 -> Int
elementsUPSel2_1 = elementsUSel2_1 . upsel2_usel
{-# INLINE elementsUPSel2_1 #-}


-- | O(1). Take the sequential `USel2` from a `UPSel2`.
selUPSel2 :: UPSel2 -> USel2
selUPSel2       = upsel2_usel
{-# INLINE selUPSel2 #-}


-- | O(1). Take the `UPSelRep2` from a `UPSel2`.
repUPSel2 :: UPSel2 -> UPSelRep2
repUPSel2       = upsel2_rep
{-# INLINE repUPSel2 #-}


-- Representation selectors ---------------------------------------------------
-- | Computes a `UPSelRep2` from an array of tags. This is used when parallelising
--   a `combine` operation. See the docs for `UPSelRep2` for details.
mkUPSelRep2 :: Vector Tag -> UPSelRep2
mkUPSelRep2 tags = zipD idxs lens
  where
    lens = mapD   theGang count
         $ splitD theGang balanced tags

    idxs = fst
         $ scanD theGang add (0,0) lens

    count bs = let ones = US.sum (US.map tagToInt bs)
               in (US.length bs - ones,ones)

    add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
{-# INLINE_UP mkUPSelRep2 #-}


indicesUPSelRep2 :: Vector Tag -> UPSelRep2 -> Vector Int
indicesUPSelRep2 tags rep 
        = joinD theGang balanced
        $ zipWithD theGang indices
             (splitD theGang balanced tags)
              rep
  where
    indices tags' ((i,j), (m,n))
      = US.combine2ByTag tags' (US.enumFromStepLen i 1 m)
                               (US.enumFromStepLen j 1 n)
{-# INLINE_UP indicesUPSelRep2 #-}


-- | O(n). Count the number of elements to take from the first array.
elementsUPSelRep2_0 :: Vector Tag -> UPSelRep2 -> Int
elementsUPSelRep2_0 _
        = sumD theGang . fstD . sndD
{-# INLINE_UP elementsUPSelRep2_0 #-}


-- | O(n). Count the number of elements to take from the second array.
elementsUPSelRep2_1 :: Vector Tag -> UPSelRep2 -> Int
elementsUPSelRep2_1 _
        = sumD theGang . sndD . sndD
{-# INLINE_UP elementsUPSelRep2_1 #-}


-- | O(1). Construct a selector. Wrapper for `UPSel2`.
mkUPSel2 :: Vector Tag -> Vector Int -> Int -> Int -> UPSelRep2 -> UPSel2
mkUPSel2 tags is n0 n1 rep
        = UPSel2 (mkUSel2 tags is n0 n1) rep
{-# INLINE_UP mkUPSel2 #-}
