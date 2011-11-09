{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential
        (extractsSU)
where
import Data.Array.Parallel.Unlifted.Sequential.Basics
import Data.Array.Parallel.Unlifted.Sequential.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Sums
import Data.Array.Parallel.Unlifted.Sequential.USegd    ()
import Data.Array.Parallel.Unlifted.Sequential.USel     ()
import Data.Array.Parallel.Unlifted.Sequential.USSegd   ()
import Data.Array.Parallel.Unlifted.Sequential.UVSegd   ()
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Vector                                    as V
import Prelude hiding (zip)


-- TODO: This isn't finished because we don't have a sequential version of
--       USegd.replicateWith / segmented replicate. There is a corresponding
--       version of extractsSU in the parallel prim library.
{-# INLINE_U extractsSU #-}
extractsSU
        :: Unbox a 
        => V.Vector (Vector a) 
        -> Vector Int  -- source ids
        -> Vector Int  -- base indices
        -> Vector Int  -- segment lengths
        -> Vector a

extractsSU = error "Data.Array.Parallel.Unlifted.Sequential.extractsSU: not implemented"

{-
extractsSU arrs srcids ixBase lens 
 = let -- total length of the result
        dstLen    = sumSU lens
        segd      = USegd.fromLengths lens
    
        -- source array ids to load from
        srcids'   = USegd.replicateWithP segd srcids

        -- base indices in the source array to load from
        baseixs   = USegd.replicateWithP segd ixBase
        
        -- starting indices for each of the segments
        startixs  = U.scanl (+) 0 lens
          
        -- starting indices for each of the segments in the result
        startixs' = USegd.replicateWithP segd startixs

        {-# INLINE get #-}
        get (ixDst, ixSegDst) (ixSegSrcBase, srcid)
         = let  !arr    = arrs V.! srcid                        -- TODO: use unsafeIndex
                !ix     = ixDst - ixSegDst + ixSegSrcBase
           in   arr U.! ix                                      -- TODO unsafe unsafeIndex
         
        result    = U.zipWith get
                        (zip (U.enumFromTo 0 (dstLen - 1))
                                 startixs')
                        (zip baseixs
                                 srcids')
   in result
-}