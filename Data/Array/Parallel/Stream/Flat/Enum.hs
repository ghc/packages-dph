-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Basics
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
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
  enumFromStepLenS
) where

import Data.Array.Parallel.Base (
  (:*:)(..))
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

