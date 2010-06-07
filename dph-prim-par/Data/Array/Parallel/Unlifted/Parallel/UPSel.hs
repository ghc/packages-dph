-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.UPSel
-- Copyright   : (c) 2010         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Parallel selectors.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.UPSel (

  -- * Types
  UPSel2,

  -- * Operations on segment descriptors
  tagsUPSel2, indicesUPSel2, elementsUPSel2_0, elementsUPSel2_1,
  selUPSel2, distUPSel2,
  mkUPSel2
) where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Base ((:*:)(..), fstS)

data UPSel2 = UPSel2 { upsel2_usel :: !USel2
                       -- (offset as :*: offset bs) :*: (length as :*: length bs)
                     , upsel2_dist :: Dist ((Int :*: Int) :*: (Int :*: Int))
                     }

tagsUPSel2 :: UPSel2 -> UArr Int
{-# INLINE tagsUPSel2 #-}
tagsUPSel2 = tagsUSel2 .  upsel2_usel

indicesUPSel2 :: UPSel2 -> UArr Int
{-# INLINE indicesUPSel2 #-}
indicesUPSel2 = indicesUSel2 . upsel2_usel

elementsUPSel2_0 :: UPSel2 -> Int
{-# INLINE elementsUPSel2_0 #-}
elementsUPSel2_0 = elementsUSel2_0 . upsel2_usel

elementsUPSel2_1 :: UPSel2 -> Int
{-# INLINE elementsUPSel2_1 #-}
elementsUPSel2_1 = elementsUSel2_1 . upsel2_usel

selUPSel2 :: UPSel2 -> USel2
{-# INLINE selUPSel2 #-}
selUPSel2 = upsel2_usel

distUPSel2 :: UPSel2 -> Dist ((Int :*: Int) :*: (Int :*: Int))
{-# INLINE distUPSel2 #-}
distUPSel2 = upsel2_dist

mkUPSel2 :: UArr Int -> UArr Int -> Int -> Int -> UPSel2
{-# INLINE mkUPSel2 #-}
mkUPSel2 tags is n0 n1 = toUPSel2 (mkUSel2 tags is n0 n1)

toUPSel2 :: USel2 -> UPSel2
{-# INLINE toUPSel2 #-}
toUPSel2 sel = UPSel2 sel (zipD idxs lens)
  where
    tags = tagsUSel2 sel

    lens = mapD  theGang count
         $ splitD theGang balanced tags

    idxs = fstS
         $ scanD theGang add (0 :*: 0) lens

    count bs = let ones = sumU bs
               in (lengthU bs - ones) :*: ones

    add (x1 :*: y1) (x2 :*: y2) = (x1+x2) :*: (y1+y2)
