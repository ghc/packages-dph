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
  USegd,

  -- * Operations on segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd, elementsUSegd,
  lengthsToUSegd, mkUSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Sums
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Text ()

