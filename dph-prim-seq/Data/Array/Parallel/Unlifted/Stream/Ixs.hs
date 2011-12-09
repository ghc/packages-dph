{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"
module Data.Array.Parallel.Unlifted.Stream.Ixs
        ( unsafeStreamSrcIxsThroughVSegids
        , unsafeStreamSrcIxsThroughUSSegd)
where
import Data.Vector.Fusion.Stream.Monadic
import Data.Array.Parallel.Unlifted.Sequential.USSegd            (USSegd(..))
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd  as USSegd
import qualified Data.Vector.Unboxed                             as U


-- VSegd Streamers ------------------------------------------------------------
-- | Take a stream of virtual segment and segment element indices,
--   and convert it to a stream of physical segment and segment element indices.
unsafeStreamSrcIxsThroughVSegids
        :: Monad m
        => U.Vector Int -> Stream m (Int, Int) -> Stream m (Int, Int)

unsafeStreamSrcIxsThroughVSegids vsegids (Stream mkStep s0 size0)
 = vsegids `seq` Stream mkStep' s0 size0
 where
        {-# INLINE_INNER mkStep' #-}
        mkStep' s
         = do   step    <- mkStep s
                case step of
                 Yield (ix1, ix2) s'
                  -> let !pseg  = U.unsafeIndex vsegids ix1
                     in  return $ Yield (pseg, ix2) s'
                 
                 Skip s' -> return $ Skip s'
                 Done    -> return Done
{-# INLINE_STREAM unsafeStreamSrcIxsThroughVSegids #-}


-- SSegd Streamers ------------------------------------------------------------
-- | Take a stream of segment and segment element indices,
--   and convert it to a stream of chunk and chunk element indices.
unsafeStreamSrcIxsThroughUSSegd 
        :: Monad m
        => USSegd -> Stream m (Int, Int) -> Stream m (Int, Int)
        
unsafeStreamSrcIxsThroughUSSegd ussegd (Stream mkStep s0 size0)
 = ussegd `seq` Stream mkStep' s0 size0
 where
        !sources = USSegd.takeSources ussegd
        !starts  = USSegd.takeStarts  ussegd
   
        {-# INLINE_INNER mkStep' #-}
        mkStep' s
         = do   step    <- mkStep s
                case step of
                 Yield (ix1, ix2) s'
                  -> let !src    = U.unsafeIndex sources ix1
                         !start  = U.unsafeIndex starts  ix1
                     in  return $ Yield (src, start + ix2) s'
                 
                 Skip s' -> return $ Skip s'
                 Done    -> return Done
{-# INLINE_STREAM unsafeStreamSrcIxsThroughUSSegd #-}
