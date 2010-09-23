-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Subarrays
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Subarrays of flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.Subarrays (
  dropUP
--  Seq.slice, extractU, tailU, takeU, dropU, splitAtU,
  {- takeWhileU, dropWhileU, spanU, breakU -}
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed


dropUP :: Unbox e => Int -> Vector e -> Vector e
dropUP n xs = Seq.slice xs (min (max 0 n) (Seq.length xs)) (min (Seq.length xs) (Seq.length xs - n)) 
{-# INLINE_U dropUP #-}
{-
dropUP n xs = joinD theGang unbalanced $ (mapD theGang (\t -> Seq.slice (fstS t) (fstS $ sndS t) (sndS $ sndS t))) args
  -- joinD theGang balanced $ mapD theGang (Seq.replicate 1)  startInds
  where
    args:: Dist (Vector Int :*: (Int :*: Int))
    args = zipD (splitD theGang balanced xs) ranges

    ranges    = mapD theGang (\t -> ((max 0 (min (fstS t - 1) (n - (sndS t))))  :*: 
                                     (min (fstS t) (max 0 ((fstS t) + (sndS t) - n))))) (zipD localLen sHere) 

    sHere:: Dist Int
    sHere     = fstS $ scanD theGang (+) 0 localLen
    
    localLen:: Dist Int
    localLen  = splitLengthD  theGang xs
-}
