{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPSegd (

  -- * Types
  UPSegd,

  -- * Operations on segment descriptors
  lengthUPSegd, lengthsUPSegd, indicesUPSegd, elementsUPSegd,
  segdUPSegd, distUPSegd,
  lengthsToUPSegd, mkUPSegd
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Distributed

data UPSegd = UPSegd { upsegd_usegd :: !USegd
                     , upsegd_dsegd :: Dist ((USegd,Int),Int)
                     }


lengthUPSegd :: UPSegd -> Int
{-# INLINE lengthUPSegd #-}
lengthUPSegd = lengthUSegd . upsegd_usegd


lengthsUPSegd :: UPSegd -> Vector Int
{-# INLINE lengthsUPSegd #-}
lengthsUPSegd = lengthsUSegd . upsegd_usegd


indicesUPSegd :: UPSegd -> Vector Int
{-# INLINE indicesUPSegd #-}
indicesUPSegd = indicesUSegd . upsegd_usegd


elementsUPSegd :: UPSegd -> Int
{-# INLINE elementsUPSegd #-}
elementsUPSegd = elementsUSegd . upsegd_usegd


segdUPSegd :: UPSegd -> USegd
{-# INLINE segdUPSegd #-}
segdUPSegd = upsegd_usegd


distUPSegd :: UPSegd -> Dist ((USegd,Int),Int)
{-# INLINE distUPSegd #-}
distUPSegd = upsegd_dsegd


lengthsToUPSegd :: Vector Int -> UPSegd
{-# INLINE lengthsToUPSegd #-}
lengthsToUPSegd = toUPSegd . lengthsToUSegd


mkUPSegd :: Vector Int -> Vector Int -> Int -> UPSegd
{-# INLINE mkUPSegd #-}
mkUPSegd lens idxs n = toUPSegd (mkUSegd lens idxs n)


toUPSegd :: USegd -> UPSegd
{-# INLINE toUPSegd #-}
toUPSegd segd = UPSegd segd (splitSegdOnElemsD theGang segd)

