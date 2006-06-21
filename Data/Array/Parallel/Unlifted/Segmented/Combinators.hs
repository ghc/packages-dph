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
) where

import Data.Array.Parallel.Base (
  sndS)
import Data.Array.Parallel.Stream (
  Stream, SStream, mapS, foldValuesSS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr,
  unstreamU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr)
import Data.Array.Parallel.Unlifted.Segmented.Stream (
  streamSU)

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSU #-}
foldlSU f z = unstreamU . foldlSS f z . streamSU

foldlSS :: (b -> a -> b) -> b -> SStream a -> Stream b
{-# INLINE [1] foldlSS #-}
foldlSS f z = mapS sndS . foldValuesSS (flip f) z

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

