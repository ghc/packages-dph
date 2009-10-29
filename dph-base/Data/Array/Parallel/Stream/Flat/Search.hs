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
import Data.Array.Parallel.Base.DTrace

findS :: (a -> Bool) -> Stream a -> Maybe a
{-# INLINE_STREAM findS #-}
findS p (Stream next s _ c) = traceLoopEntry c' $ go s
  where
    go s = case next s of
             Yield x s' | p x       -> traceLoopExit c' $ Just x
                        | otherwise -> go s'
             Skip    s'             -> go s'
             Done                   -> traceLoopExit c' Nothing

    c' = "findS" `sArgs` c

findIndexS :: (a -> Bool) -> Stream a -> Maybe Int
{-# INLINE_STREAM findIndexS #-}
findIndexS p (Stream next s _ c) = traceLoopEntry c' $ go 0 s
  where
    go i s = case next s of
               Yield x s' | p x       -> traceLoopExit c' $ Just i
                          | otherwise -> go (i+1) s'
               Skip    s'             -> go i     s'
               Done                   -> traceLoopExit c' Nothing

    c' = "findIndexS" `sArgs` c

