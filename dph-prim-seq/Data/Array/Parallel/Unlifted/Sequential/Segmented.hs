-- | Interface to operations on segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented (

  replicateSU, replicateRSU, appendSU, indicesSU, indicesSU',

  foldlSU, foldSU, fold1SU,
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
  
  -- * Slice Segment Descriptors
  USSegd, mkUSSegd, validUSSegd,
  emptyUSSegd, singletonUSSegd,
  promoteUSegdToUSSegd,
  lengthUSSegd, lengthsUSSegd, indicesUSSegd,
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

