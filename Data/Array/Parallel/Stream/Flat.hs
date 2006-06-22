-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Flat streams
--

module Data.Array.Parallel.Stream.Flat (
  Step(..), Stream(..),

  emptyS, replicateS, replicateEachS, (+++),
  enumFromToS, enumFromThenToS,
  toStream, fromStream,

  mapS, filterS, foldS, scanS,
  zipWithS, zipWith3S, zipS
) where

import Data.Array.Parallel.Stream.Flat.Stream
import Data.Array.Parallel.Stream.Flat.Basics
import Data.Array.Parallel.Stream.Flat.Combinators

