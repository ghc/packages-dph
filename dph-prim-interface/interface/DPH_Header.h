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
  fold,  fold_s,  fold_ss, fold_r,
  fold1, fold1_s, fold1_ss,
  sum,   sum_s,   sum_ss,  sum_r,
  count, count_s, count_ss,
  scan,
  and, 

  -- * Segmented Constructors
  append_s,
  replicate_s, replicate_rs, 

  -- * Segmented Projections
  indices_s,
  extract_ss,
    
  -- * Segment Descriptors
  Segd,
  mkSegd,
  validSegd,
  emptySegd,
  singletonSegd,
  lengthsToSegd,
  lengthSegd,
  lengthsSegd,
  indicesSegd,
  elementsSegd,
  plusSegd, 

  -- * Scattered Segment Descriptors
  SSegd,
  mkSSegd,
  validSSegd,
  emptySSegd,
  singletonSSegd,
  isContiguousSSegd,
  promoteSegdToSSegd,
  lengthSSegd,
  lengthsSSegd,
  indicesSSegd,
  startsSSegd,
  sourcesSSegd,
  getSegOfSSegd,
  appendSSegd,
  
  -- * Virtual Segment Descriptors
  VSegd,
  mkVSegd,
  validVSegd,
  promoteSegdToVSegd,
  promoteSSegdToVSegd,
  emptyVSegd,
  singletonVSegd,
  isManifestVSegd,
  isContiguousVSegd,
  lengthOfVSegd,
  takeVSegidsOfVSegd,   takeVSegidsRedundantOfVSegd,
  takeSSegdOfVSegd,     takeSSegdRedundantOfVSegd,
  takeLengthsOfVSegd,
  getSegOfVSegd,
  demoteToSSegdOfVSegd,
  demoteToSegdOfVSegd,
  updateVSegsOfVSegd,   updateVSegsReachableOfVSegd,
  appendVSegd,
  combine2VSegd,
  
  -- * Selectors
  Sel2, mkSel2, 
  tagsSel2, indicesSel2, elementsSel2_0, elementsSel2_1, repSel2,
  tagsToSel2,
  
  -- * Selector representations
  mkSelRep2, indicesSelRep2, elementsSelRep2_0, elementsSelRep2_1,
  
  -- * Packing and picking
  packByTag, pick,
  
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
import qualified Data.Vector    as VV
