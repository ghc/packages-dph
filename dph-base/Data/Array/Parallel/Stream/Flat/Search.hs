-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Search
-- Copyright   : (c) 2007 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
--
-- Searching in streams
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Stream.Flat.Search (
  findS, findIndexS
) where

import Data.Array.Parallel.Stream.Flat.Stream

findS :: (a -> Bool) -> Stream a -> Maybe a
{-# INLINE_STREAM findS #-}
findS p (Stream next s _) = go s
  where
    go s = case next s of
             Yield x s' | p x       -> Just x
                        | otherwise -> go s'
             Skip    s'             -> go s'
             Done                   -> Nothing

findIndexS :: (a -> Bool) -> Stream a -> Maybe Int
{-# INLINE_STREAM findIndexS #-}
findIndexS p (Stream next s _) = go 0 s
  where
    go i s = case next s of
               Yield x s' | p x       -> Just i
                          | otherwise -> go (i+1) s'
               Skip    s'             -> go i     s'
               Done                   -> Nothing

