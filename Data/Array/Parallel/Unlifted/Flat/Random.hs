-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Random
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
--  Random generation of flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Random
where

import Data.Array.Parallel.Stream (
  randomS, randomRS)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Flat.Stream (
  unstreamU)

import System.Random

randomU :: (UA a, Random a, RandomGen g) => Int -> g -> UArr a
{-# INLINE randomU #-}
randomU n = unstreamU . randomS n

randomRU :: (UA a, Random a, RandomGen g) => Int -> (a,a) -> g -> UArr a
{-# INLINE randomRU #-}
randomRU n r = unstreamU . randomRS n r


