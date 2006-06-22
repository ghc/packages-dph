-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Segmented
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Segmented streams. This module will go away. 
--

module Data.Array.Parallel.Stream.Segmented (
  SStream(..),
  segmentS, foldValuesSS
) where

import Data.Array.Parallel.Base (
  (:*:)(..), (:+:)(..))
import Data.Array.Parallel.Stream.Flat (
  Step(..), Stream(..))

-- | The type of segmented streams. The start of each segment is marked with
-- @'Inl' n@, where @n@ is the length of the segment.
--
data SStream a = forall s.
                 SStream !(s -> Step s (Int :+: a)) !s !Int !Int

-- | Create a segmented stream from a stream of segment lengths and a stream
-- of values.
--
segmentS :: Stream Int -> Stream a -> SStream a
{-# INLINE [1] segmentS #-}
segmentS (Stream nextseg s nsegs)
         (Stream nextval t nvals) =
  SStream next (0 :*: s :*: t) nsegs nvals
  where
    next (0 :*: s :*: t) =
      case nextseg s of
        Done       -> Done
        Skip    s' -> Skip  (0 :*: s' :*: t)
        Yield n s' -> Yield (Inl n) (n :*: s' :*: t)
    next (n :*: s :*: t) =
      case nextval t of
        Done       -> Done
        Skip    t' -> Skip (n :*: s :*: t')
        Yield x t' -> Yield (Inr x) (n-1 :*: s :*: t')

-- | Fold each segment of a segmented stream, producing a flat stream of
-- segment lengths and results.
--
foldValuesSS :: (a -> r -> r) -> r -> SStream a -> Stream (Int :*: r)
{-# INLINE [1] foldValuesSS #-}
foldValuesSS f z (SStream next s nsegs _) =
  Stream next' (init s) nsegs
  where
    init s = case next s of
               Done             -> (False :*: 0 :*: z :*: s)
               Skip          s' -> init s'
               Yield (Inr _) s' -> init s'
               Yield (Inl n) s' -> (True :*: n :*: z :*: s')

    next' (False :*: m :*: r :*: s) = Done
    next' (True :*: m :*: r :*: s) =
      case next s of
        Done             -> Yield (m :*: r) (False :*: m :*: z :*: s)
        Skip s'          -> Skip            (True  :*: m :*: r :*: s')
        Yield (Inl n) s' -> Yield (m :*: r) (True  :*: n :*: z :*: s')
        Yield (Inr x) s' -> Skip            (True  :*: m :*: f x r :*: s')

