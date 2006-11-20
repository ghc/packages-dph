-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Random
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Random streams
--

module Data.Array.Parallel.Stream.Flat.Random (
  randomS, randomRS
) where

import Data.Array.Parallel.Base
import Data.Array.Parallel.Stream.Flat.Stream

import System.Random

randomS :: (RandomGen g, Random a) => Int -> g -> Stream a
{-# INLINE [1] randomS #-}
randomS n g = Stream step (Lazy g :*: n) n
  where
    {-# INLINE step #-}
    step (Lazy g :*: 0) = Done
    step (Lazy g :*: n) = let (x,g') = random g
                          in Yield x (Lazy g' :*: (n-1))

randomRS :: (RandomGen g, Random a) => Int -> (a,a) -> g -> Stream a
{-# INLINE [1] randomRS #-}
randomRS n r g = Stream step (Lazy g :*: n) n
  where
    {-# INLINE step #-}
    step (Lazy g :*: 0) = Done
    step (Lazy g :*: n) = let (x,g') = randomR r g
                          in Yield x (Lazy g' :*: (n-1))

