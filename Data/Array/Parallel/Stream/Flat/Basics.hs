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
  emptyS, consS, replicateS, replicateEachS, (+++),
  toStream, fromStream
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), Box(..))
import Data.Array.Parallel.Stream.Flat.Stream

-- | Empty stream
--
emptyS :: Stream a
emptyS = Stream (const Done) () 0

-- | Construction
--
consS :: a -> Stream a -> Stream a
{-# INLINE [1] consS #-}
consS x (Stream next s n) = Stream next' (JustS (Box x) :*: s) (n+1)
  where
    {-# INLINE next' #-}
    next' (JustS (Box x) :*: s) = Yield x (NothingS :*: s)
    next' (NothingS      :*: s) = case next s of
                                    Yield y s' -> Yield y (NothingS :*: s')
                                    Skip    s' -> Skip    (NothingS :*: s')
                                    Done       -> Done

-- | Replication
--
replicateS :: Int -> a -> Stream a
{-# INLINE [1] replicateS #-}
replicateS n x = Stream next 0 n
  where
    {-# INLINE next #-}
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
    {-# INLINE next' #-}
    next' (0 :*: _ :*: s) =
      case next s of
        Done -> Done
        Skip s' -> Skip (0 :*: NothingS :*: s')
        Yield (k :*: x) s' -> Skip (k :*: JustS (Box x) :*: s')
    next' (k :*: JustS (Box x) :*: s) =
      Yield x (k-1 :*: JustS (Box x) :*: s)

-- | Concatenation
--
(+++) :: Stream a -> Stream a -> Stream a
{-# INLINE [1] (+++) #-}
Stream next1 s m +++ Stream next2 t n = Stream next (True :*: s :*: t) (m+n)
  where
    {-# INLINE next #-}
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

-- | Conversion to\/from lists
-- --------------------------

-- | Convert a list to a 'Stream'
--
toStream :: [a] -> Stream a
{-# INLINE [1] toStream #-}
toStream xs = Stream gen (Box xs) (length xs)
  where
    {-# INLINE gen #-}
    gen (Box [])     = Done
    gen (Box (x:xs)) = Yield x (Box xs)

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

