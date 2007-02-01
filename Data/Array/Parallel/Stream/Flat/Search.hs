-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Search
-- Copyright   : (c) 2007 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Searching in streams
--

module Data.Array.Parallel.Stream.Flat.Search (
  findS, findIndexS
) where

import Data.Array.Parallel.Base
import Data.Array.Parallel.Stream.Flat.Stream

findS :: (a -> Bool) -> Stream a -> MaybeS a
{-# INLINE [1] findS #-}
findS p (Stream next s _) = go s
  where
    go s = case next s of
             Yield x s' | p x       -> JustS x
                        | otherwise -> go s'
             Skip    s'             -> go s'
             Done                   -> NothingS

findIndexS :: (a -> Bool) -> Stream a -> MaybeS Int
{-# INLINE [1] findIndexS #-}
findIndexS p (Stream next s _) = go 0 s
  where
    go i s = case next s of
               Yield x s' | p x       -> JustS i
                          | otherwise -> go (i+1) s'
               Skip    s'             -> go i     s'
               Done                   -> NothingS

