{-# LANGUAGE MagicHash #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted (
  -- * Basics
  Elt, Array,  
  
  -- * Constructors
  empty,
  (+:+),
  generate,
  replicate, repeat,
  indexed,
  enumFromTo, enumFromThenTo, enumFromStepLen, enumFromStepLenEach,

  -- * Projections
  length,
  (!:),
  extract, drop,
  filter,
  
  -- * Permutation
  permute,
  bpermute,
  mbpermute,
  bpermuteDft,
  
  -- * Update
  update,
  
  -- * Packing and Combining
  pack,
  combine, combine2,
  interleave,

  -- * Map and ZipWith
  map, zipWith, zipWith3, zipWith4,

  -- * Zipping and Unzipping
  zip,  unzip, fsts, snds,
  zip3, unzip3,
    
  -- * Folds
  fold, fold1,
  and, sum, scan,

  -- * Segmented Constructors
  append_s, replicate_s, replicate_rs, 

  -- * Segmented Projections
  indices_s,

  -- * Segmented Folds
  fold_s, fold1_s, fold_r, sum_s,  sum_r,
  
  -- * Segment Descriptors
  Segd, mkSegd,
  emptySSegd, singletonSSegd,
  lengthSegd, lengthsSegd, indicesSegd, elementsSegd, lengthsToSegd,
  plusSegd, 

  -- * Slice Segment Descriptors
  SSegd, mkSSegd, validSSegd,
  promoteSegdToSSegd,
  lengthSSegd, lengthsSSegd, indicesSSegd, sourcesSSegd, 
  getSegOfSSegd,
  appendSSegd,
  
  -- * Virtual Segment Descriptors
  VSegd, mkVSegd, validVSegd,
  emptyVSegd, singletonVSegd,
  promoteSegdToVSegd,  unsafeMaterializeVSegd,
  promoteSSegdToVSegd, demoteVSegdToSSegd,
  vsegidsVSegd, ssegdVSegd,
  lengthVSegd, lengthsVSegd,
  getSegOfVSegd,
  updateVSegsOfVSegd,
  appendVSegd, combine2VSegd,
  
  -- * Selectors
  Sel2, mkSel2, 
  tagsSel2, indicesSel2, elementsSel2_0, elementsSel2_1, repSel2,
  tagsToSel2,
  
  -- * Selector representations
  mkSelRep2, indicesSelRep2, elementsSelRep2_0, elementsSelRep2_1,
  
  -- * Packing and picking
  packByTag, pick,
  
  -- * Counting
  count, count_s,

  -- * Random arrays
  randoms, randomRs,
  
  -- * Array IO
  IOElt, hGet, hPut,
  toList, fromList,
) where

import Prelude                    (Num, Int, Bool, Float, Double)
import System.IO                  (IO, Handle)
import Data.Word                  (Word8)
import qualified System.Random
import qualified Prelude

