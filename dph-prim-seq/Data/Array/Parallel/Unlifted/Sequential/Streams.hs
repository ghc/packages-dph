{-# LANGUAGE CPP #-}
#include "fusion-phases.h"
module Data.Array.Parallel.Unlifted.Sequential.Streams
        ( unsafeStreamSegsFromNested
        , unsafeExtractsFromNestedWithUSSegd
        , unsafeStreamSegsFromVectors
        , unsafeExtractsFromVectorsWithUSSegd
        , unsafeExtractsFromVectorsWithUVSegd)
where
import Data.Vector.Unboxed                                       (Unbox,   Vector)
import Data.Array.Parallel.Unlifted.Sequential.Vectors           (Unboxes, Vectors)
import Data.Array.Parallel.Unlifted.Sequential.USSegd            (USSegd(..))
import Data.Array.Parallel.Unlifted.Sequential.UVSegd            (UVSegd(..))
import qualified Data.Array.Parallel.Unlifted.Sequential.Vectors as US
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd   as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd  as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd  as UVSegd
import qualified Data.Vector.Fusion.Stream                       as S
import qualified Data.Vector.Fusion.Stream.Size                  as S
import qualified Data.Vector.Fusion.Stream.Monadic               as M
import qualified Data.Vector.Unboxed                             as U
import qualified Data.Vector.Generic                             as G
import qualified Data.Vector                                     as V
import qualified Data.Primitive.ByteArray                        as P


-- Nested -----------------------------------------------------------------------------------------
-- | Stream some physical segments from many data arrays.
-- 
--   * TODO: make this more efficient, and fix fusion.
--           We should be able to eliminate a lot of the indexing happening in the 
--           inner loop by being cleverer about the loop state.
--
--   * TODO: If this is contiguous then we can stream the lot without worrying 
--           about jumping between segments. EXCEPT that this information must be
--           statically visible else streamSegs won't fuse, so we can't have an 
--           ifThenElse checking the manifest flag.

unsafeStreamSegsFromNested
        :: Unbox a
        => USSegd               -- ^ Segment descriptor defining segments base
                                --   on source vectors.
        -> V.Vector (Vector a)  -- ^ Source arrays.
        -> S.Stream a

unsafeStreamSegsFromNested
        ussegd@(USSegd _ starts sources usegd)
        pdatas
 = let  
        -- length of each segment
        pseglens        = USegd.takeLengths usegd
 
        -- We've finished streaming this pseg
        {-# INLINE_INNER fn #-}
        fn (pseg, ix)
         -- All psegs are done.
         | pseg >= USSegd.length ussegd
         = return $ S.Done
         
         -- Current pseg is done
         | ix   >= pseglens `U.unsafeIndex` pseg 
         = return $ S.Skip (pseg + 1, 0)

         -- Stream an element from this pseg
         | otherwise
         = let  !srcid   = sources `U.unsafeIndex` pseg
                !pdata   = pdatas  `V.unsafeIndex` srcid
                !start   = starts  `U.unsafeIndex` pseg
                !result  = pdata   `U.unsafeIndex` (start + ix)
           in   return $ S.Yield result (pseg, ix + 1)

   in   M.Stream fn (0, 0) S.Unknown
{-# INLINE_STREAM unsafeStreamSegsFromNested #-}

-- | Copy segments from a `Vectors` and concatenate them into a new array.
unsafeExtractsFromNestedWithUSSegd
        :: (U.Unbox a)
        => USSegd -> V.Vector (Vector a) -> U.Vector a

unsafeExtractsFromNestedWithUSSegd ussegd vectors
        = G.unstream $ unsafeStreamSegsFromNested ussegd vectors
{-# INLINE_U unsafeExtractsFromNestedWithUSSegd #-}


-- Vectors ----------------------------------------------------------------------------------------
-- | Stream segments from a `Vectors`.
-- 
--   * There must be at least one segment in the `USSegd`, but this is not checked.
-- 
--   * No bounds checking is done for the `USSegd`.
unsafeStreamSegsFromVectors
        :: Unboxes a
        => Maybe (U.Vector Int) -- ^ Virtual segment identifiers
                                --   if `Nothing` this is assumed to be [0, 1, 2 ... segs - 1]
        -> USSegd               -- ^ Scattered segment descriptor
        -> Vectors a            -- ^ Vectors holding source data.
        -> S.Stream a

unsafeStreamSegsFromVectors 
        mVSegids
        ussegd@(USSegd _ segStarts segSources usegd) 
        vectors
 = segStarts `seq` segSources `seq` usegd `seq` vectors `seq`
   let  -- Length of each segment
        !segLens        = USegd.takeLengths usegd

        -- Total number of segments.
        !segsTotal      = USSegd.length ussegd
 
        -- Total number of elements to stream.
        !elements       = USegd.takeElements usegd
 
        -- Convert a virtual segment id to a physical one.
        {-# INLINE toPSeg #-}
        toPSeg segid
         = case mVSegids of
                 Nothing        -> segid
                 Just vsegids   -> U.unsafeIndex vsegids segid
 
        -- seg, ix of that seg in usegd, length of seg, elem in seg
        {-# INLINE_INNER fnSeg #-}
        fnSeg (ixSeg, baSeg, ixEnd, ixElem)
         = ixSeg `seq` baSeg `seq`
           if ixElem >= ixEnd                   -- Was that the last elem in the current seg?
            then if ixSeg + 1 >= segsTotal      -- Was that last seg?

                       -- That was the last seg, we're done.
                  then return $ S.Done
                  
                       -- Move to the next seg.
                  else let ixSeg'       = ixSeg + 1
                           ixPSeg'      = toPSeg ixSeg'
                           sourceSeg    = U.unsafeIndex segSources ixPSeg'
                           startSeg     = U.unsafeIndex segStarts  ixPSeg'
                           lenSeg       = U.unsafeIndex segLens    ixPSeg'
                           (arr, startArr, lenArr) 
                                        = US.unsafeIndexUnpack vectors sourceSeg
                       in  return $ S.Skip
                                  ( ixSeg'
                                  , arr
                                  , startArr + startSeg + lenSeg
                                  , startArr + startSeg)

                 -- Stream the next element from the segment.
            else let !result  = P.indexByteArray baSeg ixElem
                 in  return   $ S.Yield result (ixSeg, baSeg, ixEnd, ixElem + 1)

        -- Starting state of the stream.
        !initState
         = let  ixPSeg    = toPSeg 0
                sourceSeg = U.unsafeIndex segSources ixPSeg
                startSeg  = U.unsafeIndex segStarts  ixPSeg
                lenSeg    = U.unsafeIndex segLens    ixPSeg
                (arr, startArr, lenArr) 
                          = US.unsafeIndexUnpack vectors sourceSeg
           in   ( 0                              -- starting segment id
                , arr                            -- starting segment data
                , startArr + startSeg + lenSeg   -- segment end
                , startArr + startSeg)           -- segment start ix

        -- It's important that we set the result stream size, so Data.Vector
        -- doesn't need to add code to grow the result when it overflows.
   in   M.Stream fnSeg initState (S.Exact elements) 
{-# INLINE_STREAM unsafeStreamSegsFromVectors #-}


-- | Copy segments from a `Vectors` and concatenate them into a new array.
unsafeExtractsFromVectorsWithUSSegd
        :: (Unboxes a, U.Unbox a)
        => USSegd -> Vectors a -> U.Vector a

unsafeExtractsFromVectorsWithUSSegd ussegd vectors
        = G.unstream $ unsafeStreamSegsFromVectors Nothing ussegd vectors
{-# INLINE_U unsafeExtractsFromVectorsWithUSSegd #-}


-- | Copy segments from a `Vectors` and concatenate them into a new array.
--
--   TODO: avoid creating vsegids if possible.
unsafeExtractsFromVectorsWithUVSegd
        :: (Unboxes a, U.Unbox a)
        => UVSegd -> Vectors a -> U.Vector a

unsafeExtractsFromVectorsWithUVSegd uvsegd vectors
        = G.unstream 
        $ unsafeStreamSegsFromVectors 
                (Just (UVSegd.takeVSegids uvsegd))
                (UVSegd.takeUSSegd  uvsegd)
                vectors
{-# INLINE_U unsafeExtractsFromVectorsWithUVSegd #-}



