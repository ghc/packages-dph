-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Search
-- Copyright   : (c) 2007         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Searching in flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Search (
  findU, findIndexU
) where

import Data.Array.Parallel.Stream (
  findS, findIndexS)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Flat.Stream (
  streamU)

findU :: UA a => (a -> Bool) -> UArr a -> Maybe a
{-# INLINE findU #-}
findU p = findS p . streamU

findIndexU :: UA a => (a -> Bool) -> UArr a -> Maybe Int
{-# INLINE findIndexU #-}
findIndexU p = findIndexS p . streamU

