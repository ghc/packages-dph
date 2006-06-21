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

