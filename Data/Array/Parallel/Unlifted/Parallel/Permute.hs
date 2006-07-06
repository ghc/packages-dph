-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Permute
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Parallel permutations for unlifted arrays
--

module Data.Array.Parallel.Unlifted.Parallel.Permute (
  bpermuteUP, updateUP
) where

import Data.Array.Parallel.Base
import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Unlifted.Distributed

bpermuteUP :: UA a => UArr a -> UArr Int -> UArr a
{-# INLINE bpermuteUP #-}
bpermuteUP as = joinD     theGang
              . bpermuteD theGang as
              . splitD    theGang

updateUP :: UA a => UArr a -> UArr (Int :*: a) -> UArr a
{-# INLINE updateUP #-}
updateUP as us = updateD theGang (splitD theGang as) (splitD theGang us)

