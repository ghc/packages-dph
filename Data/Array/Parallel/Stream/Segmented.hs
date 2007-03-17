-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Segmented
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
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
  (:*:)(..), Box(..), MaybeS(..))
import Data.Array.Parallel.Stream.Flat (
  Step(..), Stream(..))

data SStream a = SStream { segd   :: Stream Int
                         , values :: Stream a
                         }

segmentS :: Stream Int -> Stream a -> SStream a
{-# INLINE segmentS #-}
segmentS = SStream
        
foldValuesSS :: (a -> b -> a) -> a -> SStream b -> Stream a
{-# INLINE [1] foldValuesSS #-}
foldValuesSS f z (SStream (Stream nexts ss ns) (Stream nextv vs nv)) =
  Stream next (NothingS :*: Box z :*: ss :*: vs) ns
  where
    {-# INLINE next #-}
    next (NothingS :*: Box x :*: ss :*: vs) =
      case nexts ss of
        Done        -> Done
        Skip    ss' -> Skip (NothingS :*: Box x :*: ss' :*: vs)
        Yield n ss' -> Skip (JustS n  :*: Box z :*: ss' :*: vs)

    next (JustS 0 :*: Box x :*: ss :*: vs) =
      Yield x (NothingS :*: Box z :*: ss :*: vs)
    next (JustS n :*: Box x :*: ss :*: vs) =
      case nextv vs of
        Done        -> Done
                       -- FIXME
                       -- error
                       --  "Stream.Segmented.foldSS: invalid segment descriptor"
        Skip    vs' -> Skip (JustS n :*: Box x :*: ss :*: vs')
        Yield y vs' -> Skip (JustS (n-1) :*: Box (f x y) :*: ss :*: vs')

