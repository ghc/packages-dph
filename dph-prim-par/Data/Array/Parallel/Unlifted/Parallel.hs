-- | Parallel operations on unlifted arrays
module Data.Array.Parallel.Unlifted.Parallel (
  UPSel2, UPSelRep2,

  bpermuteUP, updateUP,

  enumFromToUP, enumFromThenToUP, enumFromStepLenUP, enumFromStepLenEachUP,

  mapUP, filterUP, packUP, combineUP, combine2UP,
  zipWithUP, foldUP, scanUP,

  andUP, sumUP,

  -- * Selectors
  tagsUPSel2, indicesUPSel2, elementsUPSel2_0, elementsUPSel2_1,
  selUPSel2, repUPSel2, mkUPSel2,
  mkUPSelRep2, indicesUPSelRep2, elementsUPSelRep2_0, elementsUPSelRep2_1,
 
  -- * Scattered segment descriptors
  UPSSegd, validUPSSegd,
  mkUPSSegd,
  emptyUPSSegd, singletonUPSSegd,
  promoteUPSegdToUPSSegd,
  lengthUPSSegd, lengthsUPSSegd, indicesUPSSegd, elementsUPSSegd,
  startsUPSSegd, sourcesUPSSegd,
  getSegOfUPSSegd,
  ssegdUPSSegd, distUPSSegd,
  appendUPSSegd,
) where
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Sums
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Segmented
import Data.Array.Parallel.Unlifted.Parallel.Subarrays
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd
import Data.Array.Parallel.Unlifted.Parallel.UPSel
import Data.Array.Parallel.Unlifted.Parallel.Text ()

