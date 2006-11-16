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
  mapSU, zipWithSU,
  foldlSU, foldSU, {-fold1SU,-} {-scanSU,-} {-scan1SU,-}
) where

import Data.Array.Parallel.Base (
  sndS)
import Data.Array.Parallel.Stream (
  Stream, SStream, mapS, foldValuesSS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr, mapU, zipWithU,
  unstreamU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, segdSU, (>:))
import Data.Array.Parallel.Unlifted.Segmented.Basics (
  concatSU)
import Data.Array.Parallel.Unlifted.Segmented.Stream (
  streamSU)

import Debug.Trace

mapSU :: (UA a, UA b) => (a -> b) -> SUArr a -> SUArr b
{-# INLINE mapSU #-}
mapSU f sa = segdSU sa >: mapU f (concatSU sa)

zipWithSU :: (UA a, UA b, UA c)
          => (a -> b -> c) -> SUArr a -> SUArr b -> SUArr c
{-# INLINE zipWithSU #-}
zipWithSU f sa sb = segdSU sa >: zipWithU f (concatSU sa) (concatSU sb)

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSU #-}
foldlSU f z = unstreamU . foldValuesSS f z . streamSU

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

