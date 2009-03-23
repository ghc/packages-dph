{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Permute
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
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

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, uncurryS)

bpermuteUP :: UA a => UArr a -> UArr Int -> UArr a
{-# INLINE bpermuteUP #-}
bpermuteUP as is = splitJoinD theGang (bpermuteD theGang as) is

{-
  We can't support this for arbitrary types. The problem is:
  what happens if the second array maps multiple elements to the same position?
  I don't know what the semantics is supposed to be in Nesl, the spec don't
  seem to say anything. Note that it is not sufficient to say, e.g., that it
  is unspecified which value gets written; if we have an array of pairs,
  for instance, we might well get the first and second components from
  different values.

  We could require that the second array maps at most one element to each index.
  However, this is not what is wanted most of the time, at least not in the
  algorithms I've seen.

  So we only do the update in parallel if writing an element into the array is
  atomic. Otherwise, we do a sequential update.
-}

updateUP :: forall a. UA a => UArr a -> UArr (Int :*: a) -> UArr a
{-# INLINE updateUP #-}
updateUP as us
  | hasAtomicWriteMU (undefined :: a) 
  = atomicUpdateD theGang (splitD theGang unbalanced as)
                          (splitD theGang unbalanced us)

  | otherwise
  = updateU as us

