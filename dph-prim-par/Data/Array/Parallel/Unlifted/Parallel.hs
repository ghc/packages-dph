
-- | Parallel operations on unlifted arrays
--
--   NOTE: Each of the sections in the export list corresponds to one of the
--         Parallel modules, and the names are in the same order as in those
--         modules.
--
module Data.Array.Parallel.Unlifted.Parallel (
  -- * Basics
  lengthUP,
  nullUP,
  emptyUP,
  indexedUP,
  replicateUP,
  repeatUP,
  interleaveUP,
  
  -- * Combinators
  mapUP,
  filterUP,
  packUP,
  combineUP,  combine2UP,
  zipWithUP,
  foldUP,     fold1UP,
  foldlUP,    foldl1UP,
  scanUP,
  
  -- * Enum
  enumFromToUP,
  enumFromThenToUP,
  enumFromStepLenUP,
  enumFromStepLenEachUP,
  
  -- * Permute
  bpermuteUP,
  updateUP,

  -- * Segmented
  replicateRSUP,
  appendSUP,
  foldRUP,
  sumRUP,

  -- * Subarrays
  dropUP,
  
  -- * Sums
  andUP,
  orUP,
  allUP,     anyUP,
  sumUP,     productUP,
  maximumUP, maximumByUP,
  maximumIndexByUP,  

  -- * Selector Types
  UPSel2,

  -- * Segment Descriptor Types
  UPSegd, UPSSegd,
) where
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.Segmented
import Data.Array.Parallel.Unlifted.Parallel.Text       ()
import Data.Array.Parallel.Unlifted.Parallel.Subarrays
import Data.Array.Parallel.Unlifted.Parallel.Sums
import Data.Array.Parallel.Unlifted.Parallel.UPSel      (UPSel2)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd     (UPSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd    (UPSSegd)

