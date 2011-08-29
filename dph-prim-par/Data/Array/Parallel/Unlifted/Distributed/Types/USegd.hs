{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Types.USegd (
        lengthUSegdD,
        lengthsUSegdD,
        indicesUSegdD,
        elementsUSegdD
) where
import Data.Array.Parallel.Unlifted.Distributed.Types.Vector
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Control.Monad
import Prelude                          as P


instance DT USegd where
  data Dist  USegd   
        = DUSegd  !(Dist (Vector Int))
                  !(Dist (Vector Int))
                  !(Dist Int)

  data MDist USegd s 
        = MDUSegd !(MDist (Vector Int) s)
                  !(MDist (Vector Int) s)
                  !(MDist Int        s)

  indexD (DUSegd lens idxs eles) i
   = mkUSegd (indexD lens i) (indexD idxs i) (indexD eles i)

  newMD g
   = liftM3 MDUSegd (newMD g) (newMD g) (newMD g)

  readMD (MDUSegd lens idxs eles) i
   = liftM3 mkUSegd (readMD lens i) (readMD idxs i) (readMD eles i)

  writeMD (MDUSegd lens idxs eles) i segd
   = do writeMD lens i (lengthsUSegd  segd)
        writeMD idxs i (indicesUSegd  segd)
        writeMD eles i (elementsUSegd segd)

  unsafeFreezeMD (MDUSegd lens idxs eles)
   = liftM3 DUSegd (unsafeFreezeMD lens)
                   (unsafeFreezeMD idxs)
                   (unsafeFreezeMD eles)

  deepSeqD segd z
   = deepSeqD (lengthsUSegd  segd)
   $ deepSeqD (indicesUSegd  segd)
   $ deepSeqD (elementsUSegd segd) z

  sizeD  (DUSegd  _ _ eles) = sizeD eles
  sizeMD (MDUSegd _ _ eles) = sizeMD eles

  measureD segd 
   = "Segd " P.++ show (lengthUSegd segd) P.++ " " P.++ show (elementsUSegd segd)


-- | O(1). Yield the overall number of segments.
lengthUSegdD :: Dist USegd -> Dist Int
{-# INLINE_DIST lengthUSegdD #-}
lengthUSegdD (DUSegd lens _ _) 
        = lengthD lens


-- | O(1). Yield the lengths of the individual segments.
lengthsUSegdD :: Dist USegd -> Dist (Vector Int)
{-# INLINE_DIST lengthsUSegdD #-}
lengthsUSegdD (DUSegd lens _ _ )
        = lens


-- | O(1). Yield the segment indices of a segment descriptor.
indicesUSegdD :: Dist USegd -> Dist (Vector Int)
{-# INLINE_DIST indicesUSegdD #-}
indicesUSegdD (DUSegd _ idxs _)
        = idxs


-- | O(1). Yield the number of data elements.
elementsUSegdD :: Dist USegd -> Dist Int
{-# INLINE_DIST elementsUSegdD #-}
elementsUSegdD (DUSegd _ _ dns)
        = dns

