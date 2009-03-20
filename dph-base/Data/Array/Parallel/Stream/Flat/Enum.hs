-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Enum
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
--
-- Enum-related algorithms for streams.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Stream.Flat.Enum (
  enumFromToS, enumFromThenToS,
  enumFromStepLenS,

  enumFromToEachS,
  enumFromStepLenEachS
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..))
import Data.Array.Parallel.Stream.Flat.Stream
import Data.Array.Parallel.Stream.Flat.Combinators (
  mapS)

-- | Yield an enumerated stream
--

-- FIXME: Can this be implemented polymorphically? We could just use
-- enumFromThenTo here, but this won't really work for parallel arrays.
-- Perhaps we have to introduce an EnumP class?
--
enumFromToS :: Int -> Int -> Stream Int
{-# INLINE_STREAM enumFromToS #-}
enumFromToS start end
  = Stream step start (max 0 (end - start + 1))
  where
    {-# INLINE step #-}
    step s | s > end   = Done
           | otherwise = Yield s (s+1)

-- | Yield an enumerated stream using a specific step
--
--
enumFromThenToS :: Int -> Int -> Int -> Stream Int
{-# INLINE_STREAM enumFromThenToS #-}
enumFromThenToS start next end
  = enumFromStepLenS start delta len
  where
    delta = next - start
    diff  = end - start

    len | start < next && start <= end = ((end-start) `div` delta) + 1
        | start > next && start >= end = ((start-end) `div` (start-next)) + 1
        | otherwise                    = 0

enumFromStepLenS :: Int -> Int -> Int -> Stream Int
{-# INLINE_STREAM enumFromStepLenS #-}
enumFromStepLenS s !d n = Stream step (s :*: n) n
  where
    step (s :*: 0) = Done
    step (s :*: n) = Yield s ((s+d) :*: (n-1))

-- | @enumFromToEachS [k1 :*: m1, ..., kn :*: mn] = [k1,...,m1,...,kn,...,mn]@

-- FIXME: monomorphic for now because we need Rebox a otherwise!
--
enumFromToEachS :: Int -> Stream (Int :*: Int) -> Stream Int
{-# INLINE_STREAM enumFromToEachS #-}
enumFromToEachS n (Stream next s _) = Stream next' (NothingS :*: s) n
  where
    {-# INLINE next' #-}
    next' (NothingS :*: s)
      = case next s of
          Yield (k :*: m) s' -> Skip (JustS (k :*: m) :*: s')
          Skip            s' -> Skip (NothingS        :*: s')
          Done               -> Done

    next' (JustS (k :*: m) :*: s)
      | k > m     = Skip    (NothingS          :*: s)
      | otherwise = Yield k (JustS (k+1 :*: m) :*: s)

-- FIXME: monomorphic for now because we need Rebox a otherwise!
--
enumFromStepLenEachS :: Int -> Stream (Int :*: Int :*: Int) -> Stream Int 
enumFromStepLenEachS len (Stream next s n) = Stream next' (NothingS :*: s) len
  where
    {-# INLINE next' #-}
    next' (NothingS :*: s) 
      = case next s of 
          Yield (from :*: step :*: len) s' -> Skip (JustS (from :*: step :*: len) :*: s')
          Skip            s' -> Skip (NothingS        :*: s')
          Done               -> Done

    next' (JustS (from :*: step :*: 0) :*: s) = Skip (NothingS :*: s)
    next' (JustS (from :*: step :*: n) :*: s) = Yield from (JustS (from+step :*: step :*: (n-1)) :*: s)

      