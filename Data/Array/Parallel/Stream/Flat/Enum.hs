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
  enumFromToS, enumFromThenToS
) where

import Data.Array.Parallel.Stream.Flat.Stream

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
{-# INLINE [1] enumFromThenToS #-}
enumFromThenToS start next end = Stream step start' len
  where
    start' = fromEnum start
    next'  = fromEnum next
    end'   = fromEnum end
    delta  = next' - start'
    len    = abs (end' - start' + delta) `div` abs delta
    --
    step x | x > end'  = Done
           | otherwise = Yield (toEnum x) (x + delta)

