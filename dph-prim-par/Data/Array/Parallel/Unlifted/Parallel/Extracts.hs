{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Extracts 
        ( -- * Scattered indexing
          indexsFromVectorsUPVSegd

          -- * Scattered extracts
        , extractsFromNestedUPSSegd
        , extractsFromVectorsUPSSegd
        , extractsFromVectorsUPVSegd)
where
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd                    (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPVSegd                    (UPVSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   as Seq
import Data.Array.Parallel.Unlifted.Vectors                             (Vectors)
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd          as UPSSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPVSegd          as UPVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd         as UVSegd
import qualified Data.Array.Parallel.Unlifted.Vectors                   as US
import qualified Data.Array.Parallel.Unlifted.Stream                    as US
import qualified Data.Array.Parallel.Unlifted.Sequential                as Seq
import qualified Data.Vector                                            as V


-- Indexvs --------------------------------------------------------------------
-- | Lookup elements from some `Vectors` through a `UPVSegd`.
--
--   TODO: make this parallel.
--
indexsFromVectorsUPVSegd 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UPVSegd -> Vector (Int, Int) -> Vector a

indexsFromVectorsUPVSegd vectors upvsegd vsrcixs
 = let  -- Because we're just doing indexing here, we don't need the culled
        -- vsegids or ussegd, and can just use the redundant version.
        !vsegids  = UPVSegd.takeVSegidsRedundant upvsegd
        !upssegd  = UPVSegd.takeUPSSegdRedundant upvsegd
        !ussegd   = UPSSegd.takeUSSegd upssegd
   in   Seq.unstream
         $ US.streamElemsFromVectors        vectors
         $ US.streamSrcIxsThroughUSSegd  ussegd
         $ US.streamSrcIxsThroughVSegids vsegids
         $ Seq.stream vsrcixs
{-# INLINE_U indexsFromVectorsUPVSegd #-}


-- Extracts -------------------------------------------------------------------
-- | Copy segments from a nested vectors and concatenate them into a new array.
extractsFromNestedUPSSegd
        :: Unbox a
        => UPSSegd -> V.Vector (Vector a) -> Vector a

extractsFromNestedUPSSegd upssegd vectors
        = Seq.unstream 
        $ US.streamSegsFromNestedUSSegd
                vectors
                (UPSSegd.takeUSSegd upssegd)
{-# INLINE_U extractsFromNestedUPSSegd #-}


-- | TODO: make this parallel.
extractsFromVectorsUPSSegd
        :: (Unbox a, US.Unboxes a)
        => UPSSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUPSSegd upssegd vectors
        = Seq.extractsFromVectorsUSSegd
                (UPSSegd.takeUSSegd upssegd) 
                vectors
{-# INLINE_UP extractsFromVectorsUPSSegd #-}


-- | TODO: make this parallel.
extractsFromVectorsUPVSegd
        :: (Unbox a, US.Unboxes a)
        => UPVSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUPVSegd upvsegd vectors
        = Seq.unstream 
        $ US.streamSegsFromVectorsUVSegd vectors
        $ UVSegd.mkUVSegd 
                (UPVSegd.takeVSegidsRedundant upvsegd)
                (UPSSegd.takeUSSegd $ UPVSegd.takeUPSSegdRedundant upvsegd)
{-# INLINE_UP extractsFromVectorsUPVSegd #-}

