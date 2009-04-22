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
--  sliceU, extractU, tailU, takeU, dropU, splitAtU,
  {- takeWhileU, dropWhileU, spanU, breakU -}
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, uncurryS)


import Data.Array.Parallel.Unlifted.Sequential (
  UA, UArr, unitsU, lengthU, newU, sliceU, fstU, sndU,
  foldU, mapU, zipU, unzipU, scanU,
  indexedU, replicateU)
import Data.Array.Parallel.Unlifted.Distributed


dropUP :: UA e => Int -> UArr e -> UArr e
dropUP n xs = sliceU xs (min (max 0 n) (lengthU xs)) (min (lengthU xs) (lengthU xs - n)) 
{-# INLINE_U dropUP #-}
{-
dropUP n xs = joinD theGang unbalanced $ (mapD theGang (\t -> sliceU (fstS t) (fstS $ sndS t) (sndS $ sndS t))) args
  -- joinD theGang balanced $ mapD theGang (replicateU 1)  startInds
  where
    args:: Dist (UArr Int :*: (Int :*: Int))
    args = zipD (splitD theGang balanced xs) ranges

    ranges    = mapD theGang (\t -> ((max 0 (min (fstS t - 1) (n - (sndS t))))  :*: 
                                     (min (fstS t) (max 0 ((fstS t) + (sndS t) - n))))) (zipD localLen sHere) 

    sHere:: Dist Int
    sHere     = fstS $ scanD theGang (+) 0 localLen
    
    localLen:: Dist Int
    localLen  = splitLengthD  theGang xs
-}
