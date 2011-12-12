{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"
module Data.Array.Parallel.Unlifted.Stream.Segments
        ( streamSegsFromNestedUSSegd
        , streamSegsFromVectorsUSSegd)
where
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Unboxed                                       (Unbox,   Vector)
import Data.Array.Parallel.Unlifted.Vectors                      (Unboxes, Vectors)
import Data.Array.Parallel.Unlifted.Sequential.USSegd            (USSegd(..))
import qualified Data.Array.Parallel.Unlifted.Vectors            as US
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd   as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd  as USSegd
import qualified Data.Vector.Unboxed                             as U
import qualified Data.Vector                                     as V
import qualified Data.Primitive.ByteArray                        as P
import System.IO.Unsafe

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

streamSegsFromNestedUSSegd
        :: (Unbox a, Monad m)
        => V.Vector (Vector a)  -- ^ Source arrays.
        -> USSegd               -- ^ Segment descriptor defining segments base on source vectors.
        -> Stream m a

streamSegsFromNestedUSSegd
        pdatas
        ussegd@(USSegd _ starts sources usegd)
 = let  
        -- length of each segment
        pseglens        = USegd.takeLengths usegd
 
        -- We've finished streaming this pseg
        {-# INLINE_INNER fn #-}
        fn (pseg, ix)
         -- All psegs are done.
         | pseg >= USSegd.length ussegd
         = return $ Done
         
         -- Current pseg is done
         | ix   >= pseglens `U.unsafeIndex` pseg 
         = return $ Skip (pseg + 1, 0)

         -- Stream an element from this pseg
         | otherwise
         = let  !srcid   = sources `U.unsafeIndex` pseg
                !pdata   = pdatas  `V.unsafeIndex` srcid
                !start   = starts  `U.unsafeIndex` pseg
                !result  = pdata   `U.unsafeIndex` (start + ix)
           in   return $ Yield result (pseg, ix + 1)

   in   Stream fn (0, 0) Unknown
{-# INLINE_STREAM streamSegsFromNestedUSSegd #-}


-- Vectors ----------------------------------------------------------------------------------------
-- | Stream segments from a `Vectors`.
-- 
--   * There must be at least one segment in the `USSegd`, but this is not checked.
-- 
--   * No bounds checking is done for the `USSegd`.
--
streamSegsFromVectorsUSSegd
        :: (Unboxes a, Monad m)
        => Vectors a            -- ^ Vectors holding source data.
        -> USSegd               -- ^ Scattered segment descriptor
        -> Stream m a

streamSegsFromVectorsUSSegd
        vectors
        ussegd@(USSegd _ segStarts segSources usegd) 
 = segStarts `seq` segSources `seq` usegd `seq` vectors `seq`
   let  -- Length of each segment
        !segLens        = USegd.takeLengths usegd

        -- Total number of segments.
        !segsTotal      = USSegd.length ussegd
 
        -- Total number of elements to stream.
        !elements       = USegd.takeElements usegd
 
        -- seg, ix of that seg in usegd, length of seg, elem in seg
        {-# INLINE_INNER fnSeg #-}
        fnSeg (ixSeg, baSeg, ixEnd, ixElem)
         = ixSeg `seq` baSeg `seq`
           if ixElem >= ixEnd                   -- Was that the last elem in the current seg?
            then if ixSeg + 1 >= segsTotal      -- Was that last seg?

                       -- That was the last seg, we're done.
                  then return $ Done
                  
                       -- Move to the next seg.
                  else let ixSeg'       = ixSeg + 1
                           sourceSeg    = U.unsafeIndex segSources ixSeg'
                           startSeg     = U.unsafeIndex segStarts  ixSeg'
                           lenSeg       = U.unsafeIndex segLens    ixSeg'
                           (arr, startArr, _) 
                                        = US.unsafeIndexUnpack vectors sourceSeg
                       in  return $ Skip
                                  ( ixSeg'
                                  , arr
                                  , startArr + startSeg + lenSeg
                                  , startArr + startSeg)

                 -- Stream the next element from the segment.
            else let !result  = P.indexByteArray baSeg ixElem
                 in  return   $ Yield result (ixSeg, baSeg, ixEnd, ixElem + 1)

        -- Starting state of the stream.
        -- CAREFUL:
        --  The ussegd might not contain any segments, so we can't initialise the state
        --  just by taking the first segment length etc from the ussegd.
        --  On the other hand, we don't want to use an extra case expression to test for
        --  this sitution, as that could break fusion.
        --  Instead, start with a dummy state which forces the loop to grab the first 
        --  segment, if there are any.
        !dummy  = unsafePerformIO 
                $ P.newByteArray 0 >>= P.unsafeFreezeByteArray

        !initState
         =      ( -1    -- force fnSeg loop to load first seg
                , dummy -- dummy array data to start with
                , 0     -- force fnSeg loop to load first seg
                , 0)           

        -- It's important that we set the result stream size, so Data.Vector
        -- doesn't need to add code to grow the result when it overflows.
   in   Stream fnSeg initState (Exact elements) 
{-# INLINE_STREAM streamSegsFromVectorsUSSegd #-}

