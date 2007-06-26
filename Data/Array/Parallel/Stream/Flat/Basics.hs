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
-- Description ---------------------------------------------------------------
--
-- Basic algorithms on streams
--

module Data.Array.Parallel.Stream.Flat.Basics (
  emptyS, singletonS, consS, replicateS, replicateEachS, (+++), indexedS,
  tailS,
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
{-# INLINE [1] singletonS #-}
singletonS x = Stream next True 1
  where
    {-# INLINE next #-}
    next True  = Yield x False
    next False = Done

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

-- | Indexing
-- ----------

-- | Associate each element in the 'Stream' with its index
--
indexedS :: Stream a -> Stream (Int :*: a)
{-# INLINE [1] indexedS #-}
indexedS (Stream next s n) = Stream next' (0 :*: s) n
  where
    {-# INLINE next' #-}
    next' (i :*: s) = case next s of
                        Yield x s' -> Yield (i :*: x) ((i+1) :*: s')
                        Skip    s' -> Skip            (i     :*: s')
                        Done       -> Done

-- | Substreams
-- ------------

-- | Yield the tail of a stream
--
tailS :: Stream a -> Stream a
{-# INLINE [1] tailS #-}
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

