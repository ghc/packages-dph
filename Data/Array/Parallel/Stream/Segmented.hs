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
  (:*:)(..))
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
  Stream next (0 :*: z :*: ss :*: vs) ns
  where
    {-# INLINE next #-}
    next (0 :*: x :*: ss :*: vs) =
      case nexts ss of
        Done        -> Done
        Skip    ss' -> Skip (0 :*: x :*: ss' :*: vs)
        Yield n ss' | n == 0    -> Yield z (0 :*: z :*: ss' :*: vs)
                    | otherwise -> Skip (n :*: z :*: ss' :*: vs)
    next (n :*: x :*: ss :*: vs) =
      case nextv vs of
        Done         -> error
                          "Stream.Segmented.foldSS: invalid segment descriptor"
        Skip    vs' -> Skip (n :*: x :*: ss :*: vs')
        Yield y vs' | n == 1    -> Yield (f x y) (0 :*: z :*: ss :*: vs')
                    | otherwise -> Skip ((n-1) :*: f x y :*: ss :*: vs')

