-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
--
-- Flat streams
--

module Data.Array.Parallel.Stream.Flat (
  Step(..), Stream(..),

  emptyS, singletonS, consS, replicateS, replicateEachS, replicateEachRS, (+++),
  indexedS, tailS,
  enumFromToS, enumFromThenToS, enumFromStepLenS, enumFromToEachS, enumFromStepLenEachS,
  toStream, fromStream,

  mapS, filterS, foldS, fold1MaybeS, scanS, scan1S, mapAccumS,
  zipWithS, zipWith3S, zipS, zip3S, combineS,

  findS, findIndexS,

  randomS, randomRS
) where

import Data.Array.Parallel.Stream.Flat.Stream
import Data.Array.Parallel.Stream.Flat.Basics
import Data.Array.Parallel.Stream.Flat.Combinators
import Data.Array.Parallel.Stream.Flat.Enum
import Data.Array.Parallel.Stream.Flat.Search
import Data.Array.Parallel.Stream.Flat.Random

