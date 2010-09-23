-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.USel
-- Copyright   :  (c) 2010         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Selectors
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.USel (

  -- * Types
  USel2,

  -- * Operations on selectors
  lengthUSel2, tagsUSel2, indicesUSel2, elementsUSel2_0, elementsUSel2_1,
  mkUSel2, tagsToIndices2
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as V

-- import Data.Array.Parallel.Stream ( mapAccumS )
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic ( Stream(..) )
import Data.Array.Parallel.Base (Tag)

data USel2 = USel2 { usel2_tags      :: !(Vector Tag)
                   , usel2_indices   :: !(Vector Int)
                   , usel2_elements0 :: !Int
                   , usel2_elements1 :: !Int
                   }

lengthUSel2 :: USel2 -> Int
{-# INLINE lengthUSel2 #-}
lengthUSel2 = V.length . usel2_tags

tagsUSel2 :: USel2 -> Vector Tag
{-# INLINE tagsUSel2 #-}
tagsUSel2 = usel2_tags

indicesUSel2 :: USel2 -> Vector Int
{-# INLINE indicesUSel2 #-}
indicesUSel2 = usel2_indices

elementsUSel2_0 :: USel2 -> Int
{-# INLINE elementsUSel2_0 #-}
elementsUSel2_0 = usel2_elements0

elementsUSel2_1 :: USel2 -> Int
{-# INLINE elementsUSel2_1 #-}
elementsUSel2_1 = usel2_elements1

mkUSel2 :: Vector Tag -> Vector Int -> Int -> Int -> USel2
{-# INLINE mkUSel2 #-}
mkUSel2 = USel2

tagsToIndices2 :: Vector Tag -> Vector Int
{-# INLINE tagsToIndices2 #-}
tagsToIndices2 tags = unstream (mapAccumS add (0,0) (stream tags))
  where
    add (i,j) 0 = ((i+1,j),i)
    add (i,j) _ = ((i,j+1),j)

mapAccumS :: (acc -> a -> (acc,b)) -> acc -> S.Stream a -> S.Stream b
{-# INLINE_STREAM mapAccumS #-}
mapAccumS f acc (Stream step s n) = Stream step' (acc,s) n
  where
    {-# INLINE_INNER step' #-}
    step' (acc,s) = do
                      r <- step s
                      case r of
                        S.Yield x s' -> let (acc',y) = f acc x
                                        in
                                        return $ S.Yield y (acc',s')
                        S.Skip    s' -> return $ S.Skip (acc,s')
                        S.Done       -> return S.Done

