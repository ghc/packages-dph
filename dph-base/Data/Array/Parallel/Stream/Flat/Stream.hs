{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Stream
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
--
-- Basic types for stream-based fusion
--

module Data.Array.Parallel.Stream.Flat.Stream (
  Step(..), Stream(..)
) where

import Data.Array.Parallel.Base (
  Rebox)

data Step s a = Done
              | Skip     !s
              | Yield !a !s

instance Functor (Step s) where
  fmap f Done        = Done
  fmap f (Skip s)    = Skip s
  fmap f (Yield x s) = Yield (f x) s

data Stream a = forall s. Rebox s => Stream (s -> Step s a) !s Int

