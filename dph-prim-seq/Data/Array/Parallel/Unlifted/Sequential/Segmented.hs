{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
-- | Interface to operations on segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented (

  replicateSU, replicateRSU, appendSU, indicesSU, indicesSU',

  foldlSU,
  foldSU,   foldSSU,
  fold1SU,  fold1SSU,
  foldlRU,
  combineSU,

  -- * Logical operations
  andSU, orSU,

  -- * Arithmetic operations
  sumSU, productSU, maximumSU, minimumSU,
  sumRU,

  -- * Segment Descriptors
  USegd, mkUSegd, validUSegd,
  emptyUSegd, singletonUSegd,
  lengthUSegd, lengthsUSegd, indicesUSegd, elementsUSegd,
  lengthsToUSegd,
  
  -- * Scattered Segment Descriptors
  USSegd, mkUSSegd, validUSSegd,
  emptyUSSegd, singletonUSSegd,
  promoteUSegdToUSSegd,
  lengthUSSegd,
  usegdUSSegd, lengthsUSSegd, indicesUSSegd, elementsUSSegd,
  sourcesUSSegd, startsUSSegd,
  getSegOfUSSegd, appendUSSegd,

  -- * Virtual Segment Descriptors
  UVSegd, mkUVSegd, validUVSegd,
  emptyUVSegd, singletonUVSegd,
  promoteUSegdToUVSegd,  unsafeMaterializeUVSegd,
  promoteUSSegdToUVSegd, demoteUVSegdToUSSegd,
  vsegidsUVSegd, ussegdUVSegd,
  lengthUVSegd, lengthsUVSegd, 
  getSegOfUVSegd, updateVSegsOfUVSegd, appendUVSegd, combine2UVSegd
  
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USSegd
import Data.Array.Parallel.Unlifted.Sequential.Segmented.UVSegd
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Sums

