-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.UPSegd
-- Copyright   : (c) 2010         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Parallel segment descriptors.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.UPSegd (

  -- * Types
  UPSegd,

  -- * Operations on segment descriptors
  lengthUPSegd, lengthsUPSegd, indicesUPSegd, elementsUPSegd,
  segdUPSegd, distUPSegd,
  lengthsToUPSegd, mkUPSegd
) where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Base ((:*:))

data UPSegd = UPSegd { upsegd_usegd :: !USegd
                     , upsegd_dsegd :: Dist (USegd :*: Int :*: Int)
                     }

lengthUPSegd :: UPSegd -> Int
{-# INLINE lengthUPSegd #-}
lengthUPSegd = lengthUSegd . upsegd_usegd

lengthsUPSegd :: UPSegd -> UArr Int
{-# INLINE lengthsUPSegd #-}
lengthsUPSegd = lengthsUSegd . upsegd_usegd

indicesUPSegd :: UPSegd -> UArr Int
{-# INLINE indicesUPSegd #-}
indicesUPSegd = indicesUSegd . upsegd_usegd

elementsUPSegd :: UPSegd -> Int
{-# INLINE elementsUPSegd #-}
elementsUPSegd = elementsUSegd . upsegd_usegd

segdUPSegd :: UPSegd -> USegd
{-# INLINE segdUPSegd #-}
segdUPSegd = upsegd_usegd

distUPSegd :: UPSegd -> Dist (USegd :*: Int :*: Int)
{-# INLINE distUPSegd #-}
distUPSegd = upsegd_dsegd

lengthsToUPSegd :: UArr Int -> UPSegd
{-# INLINE lengthsToUPSegd #-}
lengthsToUPSegd = toUPSegd . lengthsToUSegd

mkUPSegd :: UArr Int -> UArr Int -> Int -> UPSegd
{-# INLINE mkUPSegd #-}
mkUPSegd lens idxs n = toUPSegd (mkUSegd lens idxs n)

toUPSegd :: USegd -> UPSegd
{-# INLINE toUPSegd #-}
toUPSegd segd = UPSegd segd (splitSegdD' theGang segd)

