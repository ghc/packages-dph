-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Basics
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
--
-- Basic algorithms on streams
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Stream.Flat.Basics (
  -- * Basic operations
  emptyS, singletonS, consS, replicateS, replicateEachS, replicateEachRS,
  (+++), indexedS,
  tailS,

  -- * Conversion to\/from lists
  toStream, fromStream
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), EitherS(..), Box(..))
import Data.Array.Parallel.Stream.Flat.Stream

-- | Empty stream
--
emptyS :: Stream a
emptyS = Stream (const Done) () 0

-- | Singleton stream
--
singletonS :: a -> Stream a
{-# INLINE_STREAM singletonS #-}
singletonS x = Stream next True 1
  where
    {-# INLINE next #-}
    next True  = Yield x False
    next False = Done

-- | Construction
--
consS :: a -> Stream a -> Stream a
{-# INLINE_STREAM consS #-}
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
{-# INLINE_STREAM replicateS #-}
replicateS n x = Stream next 0 n
  where
    {-# INLINE next #-}
    next i | i == n    = Done
           | otherwise = Yield x (i+1)

-- | Given a stream of (length,value) pairs and the sum of the lengths,
-- replicate each value to the given length.
--
replicateEachS :: Int -> Stream (Int :*: a) -> Stream a
{-# INLINE_STREAM replicateEachS #-}
replicateEachS n (Stream next s _) =
  Stream next' (0 :*: NothingS :*: s) n
  where
    {-# INLINE next' #-}
    next' (0 :*: _ :*: s) =
      case next s of
        Done -> Done
        Skip s' -> Skip (0 :*: NothingS :*: s')
        Yield (k :*: x) s' -> Skip (k :*: JustS (Box x) :*: s')
    next' (k :*: NothingS :*: s) = Done   -- FIXME: unreachable
    next' (k :*: JustS (Box x) :*: s) =
      Yield x (k-1 :*: JustS (Box x) :*: s)

-- | Repeat each element in the stream n times
--
replicateEachRS :: Int -> Stream a -> Stream a
{-# INLINE_STREAM replicateEachRS #-}
replicateEachRS !n (Stream next s m)
  = Stream next' (0 :*: NothingS :*: s) (m * n)
  where
    next' (0 :*: _ :*: s) =
      case next s of
        Done       -> Done
        Skip    s' -> Skip (0 :*: NothingS      :*: s')
        Yield x s' -> Skip (n :*: JustS (Box x) :*: s')
    next' (i :*: NothingS :*: s) = Done -- unreachable
    next' (i :*: JustS (Box x) :*: s) = Yield x (i-1 :*: JustS (Box x) :*: s)

-- | Concatenation
--
(+++) :: Stream a -> Stream a -> Stream a
{-# INLINE_STREAM (+++) #-}
Stream next1 s1 n1 +++ Stream next2 s2 n2 = Stream next (LeftS s1) (n1 + n2)
  where
    {-# INLINE next #-}
    next (LeftS s1) =
      case next1 s1 of
        Done        -> Skip    (RightS s2)
        Skip    s1' -> Skip    (LeftS  s1')
        Yield x s1' -> Yield x (LeftS  s1')

    next (RightS s2) =
      case next2 s2 of
        Done        -> Done
        Skip    s2' -> Skip    (RightS s2')
        Yield x s2' -> Yield x (RightS s2')

-- | Associate each element in the 'Stream' with its index
--
indexedS :: Stream a -> Stream (Int :*: a)
{-# INLINE_STREAM indexedS #-}
indexedS (Stream next s n) = Stream next' (0 :*: s) n
  where
    {-# INLINE next' #-}
    next' (i :*: s) = case next s of
                        Yield x s' -> Yield (i :*: x) ((i+1) :*: s')
                        Skip    s' -> Skip            (i     :*: s')
                        Done       -> Done

-- | Yield the tail of a stream
--
tailS :: Stream a -> Stream a
{-# INLINE_STREAM tailS #-}
tailS (Stream next s n) = Stream next' (False :*: s) (n-1)
  where
    {-# INLINE next' #-}
    next' (False :*: s) = case next s of
                            Yield x s' -> Skip (True  :*: s')
                            Skip    s' -> Skip (False :*: s')
                            Done       -> error "Stream.tailS: empty stream"
    next' (True  :*: s) = case next s of
                            Yield x s' -> Yield x (True :*: s')
                            Skip    s' -> Skip    (True :*: s')
                            Done       -> Done


-- | Convert a list to a 'Stream'
--
toStream :: [a] -> Stream a
{-# INLINE_STREAM toStream #-}
toStream xs = Stream gen (Box xs) (length xs)
  where
    {-# INLINE gen #-}
    gen (Box [])     = Done
    gen (Box (x:xs)) = Yield x (Box xs)

-- | Generate a list from a 'Stream'
--
fromStream :: Stream a -> [a]
{-# INLINE_STREAM fromStream #-}
fromStream (Stream next s _) = gen s
  where
    gen s = case next s of
              Done       -> []
              Skip s'    -> gen s'
              Yield x s' -> x : gen s'

