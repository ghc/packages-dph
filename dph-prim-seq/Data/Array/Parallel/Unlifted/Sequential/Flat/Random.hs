-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Random
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Random generation of flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Random
where

import Data.Array.Parallel.Stream (
  randomS, randomRS)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  unstreamU)

import System.Random

randomU :: (UA a, Random a, RandomGen g) => Int -> g -> UArr a
{-# INLINE_U randomU #-}
randomU n = unstreamU . randomS n

randomRU :: (UA a, Random a, RandomGen g) => Int -> (a,a) -> g -> UArr a
{-# INLINE_U randomRU #-}
randomRU n r = unstreamU . randomRS n r


