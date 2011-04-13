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
