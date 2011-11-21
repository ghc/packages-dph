{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel operations on unlifted arrays
--
--   * This is an internal API and shouldn't need to be used directly.
--     Client programs should use "Data.Array.Parallel.Unlifted"

--   NOTE: Each of the sections in the export list corresponds to one of the
--         Parallel modules, and the names are in the same order as in those
--         modules.
--
module Data.Array.Parallel.Unlifted.Parallel (
  -- * Basics
  lengthUP,
  nullUP,
  emptyUP,
  indexedUP,
  replicateUP,
  repeatUP,
  interleaveUP,
  
  -- * Combinators
  mapUP,
  filterUP,
  packUP,
  combineUP,  combine2UP,
  zipWithUP,
  foldUP,     fold1UP,
  foldlUP,    foldl1UP,
  scanUP,
  extractsUP,
  
  -- * Enum
  enumFromToUP,
  enumFromThenToUP,
  enumFromStepLenUP,
  enumFromStepLenEachUP,
  
  -- * Permute
  bpermuteUP,
  updateUP,

  -- * Segmented
  replicateRSUP,
  appendSUP,
  foldRUP,
  sumRUP,

  -- * Subarrays
  dropUP,
  
  -- * Sums
  andUP,
  orUP,
  allUP,     anyUP,
  sumUP,     productUP,
  maximumUP, maximumByUP,
  maximumIndexByUP
) where
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.Segmented
import Data.Array.Parallel.Unlifted.Parallel.Text       ()
import Data.Array.Parallel.Unlifted.Parallel.Subarrays
import Data.Array.Parallel.Unlifted.Parallel.Sums
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Vector                                    as V
import Data.Array.Parallel.Unlifted.Sequential.Vector           as Seq


-- | O(n). Segmented extract.

-- TODO: zip srcids ixBase and startsix before calling replicate_s
--       don't want to replicate_s multiple times on same segd.
--
-- TODO: pass in a projection function to get the correct array from the vector, 
--       to avoid unpackig all the arrays from PDatas with a big map traversal.
--
-- TODO: make this take the SSegd directly, instead of unzipped fields.
--
{-# INLINE_UP extractsUP #-}
extractsUP
        :: Unbox a 
        => V.Vector (Vector a)  -- ^ Source Vectors.
        -> Vector Int           -- ^ Source array indices for each segment.
        -> Vector Int           -- ^ Starting element for each segment in its source array.
        -> Vector Int           -- ^ Length of each segment.
        -> Vector a

extractsUP arrs srcids ixBase lens 
 = let -- total length of the result
        dstLen    = sumUP lens
        segd      = UPSegd.fromLengths lens
    
        -- source array ids to load from
        srcids'   = UPSegd.replicateWithP segd srcids

        -- base indices in the source array to load from
        baseixs   = UPSegd.replicateWithP segd ixBase
        
        -- starting indices for each of the segments
        startixs  = scanUP (+) 0 lens
          
        -- starting indices for each of the segments in the result
        startixs' = UPSegd.replicateWithP segd startixs

        {-# INLINE get #-}
        get (ixDst, ixSegDst) (ixSegSrcBase, srcid)
         = let  !arr    = arrs V.! srcid                        -- TODO: use unsafeIndex
                !ix     = ixDst - ixSegDst + ixSegSrcBase
           in   arr Seq.! ix                                    -- TODO unsafe unsafeIndex
         
        result    = zipWithUP get
                        (Seq.zip (enumFromToUP 0 (dstLen - 1))
                                 startixs')
                        (Seq.zip baseixs
                                 srcids')

   in result
