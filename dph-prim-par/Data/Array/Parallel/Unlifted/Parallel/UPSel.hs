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
  UPSel2, UPSelRep2,

  -- * Operations on segment descriptors
  tagsUPSel2, indicesUPSel2, elementsUPSel2_0, elementsUPSel2_1,
  selUPSel2, repUPSel2, mkUPSel2,
  mkUPSelRep2, indicesUPSelRep2, elementsUPSelRep2_0, elementsUPSelRep2_1,
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Base (Tag, tagToInt)


-- TODO: What is this for?
type UPSelRep2
        = Dist ((Int,Int), (Int,Int))

-- TODO: What is this for?
data UPSel2 
        = UPSel2 
        { upsel2_usel :: USel2
        , upsel2_rep  :: UPSelRep2 }


-- | O(1). Get the tags of a selector.
tagsUPSel2 :: UPSel2 -> Vector Tag
{-# INLINE tagsUPSel2 #-}
tagsUPSel2 = tagsUSel2 .  upsel2_usel


-- | O(1). Get the indices of a selector.
indicesUPSel2 :: UPSel2 -> Vector Int
{-# INLINE indicesUPSel2 #-}
indicesUPSel2 = indicesUSel2 . upsel2_usel


-- | O(1). TODO: What is this for?
elementsUPSel2_0 :: UPSel2 -> Int
{-# INLINE elementsUPSel2_0 #-}
elementsUPSel2_0 = elementsUSel2_0 . upsel2_usel


-- | O(1). TODO: What is this for?
elementsUPSel2_1 :: UPSel2 -> Int
{-# INLINE elementsUPSel2_1 #-}
elementsUPSel2_1 = elementsUSel2_1 . upsel2_usel


-- | O(1). TODO: What is this for?
selUPSel2 :: UPSel2 -> USel2
{-# INLINE selUPSel2 #-}
selUPSel2 = upsel2_usel


-- | O(1). TODO: What is this for?
repUPSel2 :: UPSel2 -> UPSelRep2
{-# INLINE repUPSel2 #-}
repUPSel2 = upsel2_rep


-- Representation selectors? --------------------------------------------------
-- TODO: How are these different from the above?

-- | TODO: What is this for?
mkUPSelRep2 :: Vector Tag -> UPSelRep2
{-# INLINE mkUPSelRep2 #-}
mkUPSelRep2 tags = zipD idxs lens
  where
    lens = mapD   theGang count
         $ splitD theGang balanced tags

    idxs = fst
         $ scanD theGang add (0,0) lens

    count bs = let ones = Seq.sum (Seq.map tagToInt bs)
               in (Seq.length bs - ones,ones)

    add (x1,y1) (x2,y2) = (x1+x2, y1+y2)


indicesUPSelRep2 :: Vector Tag -> UPSelRep2 -> Vector Int
{-# INLINE indicesUPSelRep2 #-}
indicesUPSelRep2 tags rep = joinD theGang balanced
                          $ zipWithD theGang indices
                                             (splitD theGang balanced tags)
                                              rep
  where
    indices tags ((i,j), (m,n))
      = Seq.combine2ByTag tags (Seq.enumFromStepLen i 1 m)
                               (Seq.enumFromStepLen j 1 n)


-- | O(n).
elementsUPSelRep2_0 :: Vector Tag -> UPSelRep2 -> Int
{-# INLINE elementsUPSelRep2_0 #-}
elementsUPSelRep2_0 _ = sumD theGang . fstD . sndD


-- | O(n).
elementsUPSelRep2_1 :: Vector Tag -> UPSelRep2 -> Int
{-# INLINE elementsUPSelRep2_1 #-}
elementsUPSelRep2_1 _ = sumD theGang . sndD . sndD


-- | O(1). Construct a selector. Wrapper for `UPSel2`.
mkUPSel2 :: Vector Tag -> Vector Int -> Int -> Int -> UPSelRep2 -> UPSel2
{-# INLINE mkUPSel2 #-}
mkUPSel2 tags is n0 n1 rep = UPSel2 (mkUSel2 tags is n0 n1) rep

