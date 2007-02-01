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

  emptyS, consS, replicateS, replicateEachS, (+++),
  indexedS,
  enumFromToS, enumFromThenToS, enumFromStepLenS,
  toStream, fromStream,

  mapS, filterS, foldS, fold1MaybeS, scanS, mapAccumS,
  zipWithS, zipWith3S, zipS,

  findS, findIndexS,

  randomS, randomRS
) where

import Data.Array.Parallel.Stream.Flat.Stream
import Data.Array.Parallel.Stream.Flat.Basics
import Data.Array.Parallel.Stream.Flat.Combinators
import Data.Array.Parallel.Stream.Flat.Enum
import Data.Array.Parallel.Stream.Flat.Search
import Data.Array.Parallel.Stream.Flat.Random

