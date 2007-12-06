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
-- Description ---------------------------------------------------------------
--
-- Enum-related algorithms for streams.
--

module Data.Array.Parallel.Stream.Flat.Enum (
  enumFromToS, enumFromThenToS,
  enumFromStepLenS,

  enumFromToEachS
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..))
import Data.Array.Parallel.Stream.Flat.Stream
import Data.Array.Parallel.Stream.Flat.Combinators (
  mapS)

-- | Yield an enumerated stream
--
-- FIXME: see comments about 'enumFromThenToS'
--
enumFromToS :: Enum a => a -> a -> Stream a
{-# INLINE enumFromToS #-}
enumFromToS start end = enumFromThenToS start (succ start) end

-- | Yield an enumerated stream using a specific step
--
-- FIXME: Can this be implemented at all?. Casting to 'Int' certainly doesn't
-- work for 'Float's; we have
--
-- > enumFromTo 0.5 1.4 = [0.5,1.5]
--
-- but
--
-- > enumFromToU 0.5 1.4 = [0.0,1.0]
--
-- More importantly, this won't work for types larger than 'Int'. Short of
-- introducing an EnumP class, I don't see how this can be implemented
-- efficiently.
--
-- On the other hand, we could just use enumFromThenTo here; however, this
-- won't really work for parallel arrays.
--
enumFromThenToS :: Enum a => a -> a -> a -> Stream a
{-# INLINE enumFromThenToS #-}
enumFromThenToS start next end =
  mapS toEnum (enumFromStepLenS start' delta len)
  where
    start' = fromEnum start
    next'  = fromEnum next
    end'   = fromEnum end
    delta  = next' - start'
    len    = abs (end' - start' + delta) `div` abs delta

enumFromStepLenS :: Int -> Int -> Int -> Stream Int
{-# INLINE [1] enumFromStepLenS #-}
enumFromStepLenS s d n = Stream step (s :*: n) n
  where
    step (s :*: 0) = Done
    step (s :*: n) = Yield s ((s+d) :*: (n-1))

-- enumFromToEachS [k1 :*: m1, ..., kn :*: mn] = [k1,...,m1,...,kn,...,mn]
--
-- FIXME: monomorphic for now because we need Rebox a otherwise!
--
enumFromToEachS :: Int -> Stream (Int :*: Int) -> Stream Int
{-# INLINE [1] enumFromToEachS #-}
enumFromToEachS n (Stream next s _) = Stream next' (NothingS :*: s) n
  where
    {-# INLINE next' #-}
    next' (NothingS :*: s)
      = case next s of
          Yield (k :*: m) s' -> Skip (JustS (k :*: m) :*: s')
          Skip            s' -> Skip (NothingS        :*: s')
          Done               -> Done

    next' (JustS (k :*: m) :*: s)
      | k > m     = Skip    (NothingS             :*: s)
      | otherwise = Yield k (JustS (succ k :*: m) :*: s)

