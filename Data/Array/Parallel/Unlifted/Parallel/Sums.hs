-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Sums
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Sum-like parallel combinators for unlifted arrays
--

module Data.Array.Parallel.Unlifted.Parallel.Sums (
  andUP, sumUP
) where

import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  foldUP )

andUP :: UArr Bool -> Bool
{-# INLINE andUP #-}
andUP = foldUP (&&) True

sumUP :: (UA a, DT a, Num a) => UArr a -> a
{-# INLINE sumUP #-}
sumUP = foldUP (+) 0

