{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Extracts 
        ( -- * Scattered indexing
          indexsFromVectorsWithUPVSegd

          -- * Scattered extracts
        , extractsFromNestedWithUPSSegd
        , extractsFromVectorsWithUPSSegd
        , extractsFromVectorsWithUPVSegd)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.UPSegd                     (UPSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd                    (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPVSegd                    (UPVSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   as Seq
import Data.Array.Parallel.Unlifted.Vectors                             (Vectors)
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd          as UPSSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPVSegd          as UPVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd         as UVSegd
import qualified Data.Array.Parallel.Unlifted.Vectors                   as US
import qualified Data.Array.Parallel.Unlifted.Stream                    as US
import qualified Data.Array.Parallel.Unlifted.Sequential                as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Stream.Size    ( Size(..) )
import qualified Data.Vector.Fusion.Stream                              as S
import qualified Data.Vector                                            as V


-- Indexvs --------------------------------------------------------------------
-- | Lookup elements from some `Vectors` through a `UPVSegd`.
--
--   TODO: make this parallel.
--
indexsFromVectorsWithUPVSegd 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UPVSegd -> Vector (Int, Int) -> Vector a

indexsFromVectorsWithUPVSegd vectors upvsegd vsrcixs
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
{-# INLINE_U indexsFromVectorsWithUPVSegd #-}


-- Extracts -------------------------------------------------------------------
-- | Copy segments from a nested vectors and concatenate them into a new array.
extractsFromNestedWithUPSSegd
        :: Unbox a
        => UPSSegd -> V.Vector (Vector a) -> Vector a

extractsFromNestedWithUPSSegd upssegd vectors
        = Seq.unstream 
        $ US.streamSegsFromNestedUSSegd
                vectors
                (UPSSegd.takeUSSegd upssegd)
{-# INLINE_U extractsFromNestedWithUPSSegd #-}


-- | TODO: make this parallel.
extractsFromVectorsWithUPSSegd
        :: (Unbox a, US.Unboxes a)
        => UPSSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsWithUPSSegd upssegd vectors
        = Seq.extractsFromVectorsUSSegd
                (UPSSegd.takeUSSegd upssegd) 
                vectors
{-# INLINE_UP extractsFromVectorsWithUPSSegd #-}


-- | TODO: make this parallel.
extractsFromVectorsWithUPVSegd
        :: (Unbox a, US.Unboxes a)
        => UPVSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsWithUPVSegd upvsegd vectors
        = Seq.unstream 
        $ US.streamSegsFromVectorsUVSegd vectors
        $ UVSegd.mkUVSegd 
                (UPVSegd.takeVSegidsRedundant upvsegd)
                (UPSSegd.takeUSSegd $ UPVSegd.takeUPSSegdRedundant upvsegd)
{-# INLINE_UP extractsFromVectorsWithUPVSegd #-}

