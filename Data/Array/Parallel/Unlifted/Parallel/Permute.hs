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
  bpermuteUP
) where

import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Unlifted.Distributed

bpermuteUP :: UA a => UArr a -> UArr Int -> UArr a
{-# INLINE bpermuteUP #-}
bpermuteUP as = splitJoinD theGang (bpermuteD theGang as)

{-
  I'm really not sure if we can support this. There are two problems. First,
  what happens if the second array maps multiple elements to the same position?
  I don't know what the semantics is supposed to be in Nesl, the spec don't
  seem to say anything. Note that it is not sufficient to say, e.g., that it
  is unspecified which value gets written; if we have an array of pairs,
  for instance, we might well get the first and second components from
  different values.

  So we could require that the second array maps at most one element to each
  index. This has two problems: apparently, this is not what is wanted most of
  the time (at least in the algorithms I've seen) and, more importantly, it
  still won't work with packed arrays of Bools.

updateUP :: UA a => UArr a -> UArr (Int :*: a) -> UArr a
{-# INLINE updateUP #-}
updateUP as us = updateD theGang (splitD theGang as) (splitD theGang us)
-}

