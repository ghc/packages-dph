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
-- Basic algorithms on streams
--

module Data.Array.Parallel.Stream.Flat.Basics (
  emptyS, replicateS, replicateEachS, (+++),
  enumFromToS, enumFromThenToS,
  toStream, fromStream
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..) )
import Data.Array.Parallel.Stream.Flat.Stream

-- | Empty stream
--
emptyS :: Stream a
emptyS = Stream (const Done) () 0

-- | Replication
--
replicateS :: Int -> a -> Stream a
{-# INLINE [1] replicateS #-}
replicateS n x = Stream next 0 n
  where
    next i | i == n    = Done
           | otherwise = Yield x (i+1)

-- | Given a stream of (length,value) pairs and the sum of the lengths,
-- replicate each value to the given length.
--
-- FIXME: This should probably produce a segmented stream but since we want to
-- get rid of them anyway...
--
replicateEachS :: Int -> Stream (Int :*: a) -> Stream a
{-# INLINE [1] replicateEachS #-}
replicateEachS n (Stream next s _) =
  Stream next' (0 :*: NothingS :*: s) n
  where
    next' (0 :*: _ :*: s) =
      case next s of
        Done -> Done
        Skip s' -> Skip (0 :*: NothingS :*: s')
        Yield (k :*: x) s' -> Skip (k :*: JustS x :*: s')
    next' (k :*: JustS x :*: s) =
      Yield x (k-1 :*: JustS x :*: s)

-- | Concatenation
--
(+++) :: Stream a -> Stream a -> Stream a
{-# INLINE [1] (+++) #-}
Stream next1 s m +++ Stream next2 t n = Stream next (True :*: s :*: t) (m+n)
  where
    next (True :*: s :*: t) =
      case next1 s of
        Done       -> Skip (False :*: s :*: t)
        Skip s'    -> Skip (True :*: s' :*: t)
        Yield x s' -> Yield x (True :*: s' :*: t)
    next (False :*: s :*: t) =
      case next2 t of
        Done       -> Done
        Skip t'    -> Skip (False :*: s :*: t')
        Yield x t' -> Yield x (False :*: s :*: t')

-- | Enumeration functions
-- -----------------------

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

-- | Conversion to/from lists
-- --------------------------

-- | Convert a list to a 'Stream'
--
toStream :: [a] -> Stream a
{-# INLINE [1] toStream #-}
toStream xs = Stream gen xs (length xs)
  where
    gen []     = Done
    gen (x:xs) = Yield x xs

-- | Generate a list from a 'Stream'
--
fromStream :: Stream a -> [a]
{-# INLINE [1] fromStream #-}
fromStream (Stream next s _) = gen s
  where
    gen s = case next s of
              Done       -> []
              Skip s'    -> gen s'
              Yield x s' -> x : gen s'

