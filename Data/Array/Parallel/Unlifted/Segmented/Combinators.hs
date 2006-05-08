-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Combinators
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Standard combinators for segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Combinators (
  foldlSU, foldSU, {-fold1SU,-} {-scanSU,-} {-scan1SU,-}
  loopSU
) where

import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr)
import Data.Array.Parallel.Unlifted.Segmented.Loop (
  loopSU)
import Data.Array.Parallel.Unlifted.Segmented.Fusion (
  foldEFL, keepSFL, loopAccS)

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSU #-}
foldlSU f z = loopAccS . loopSU (foldEFL f) (keepSFL (const z)) z

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

