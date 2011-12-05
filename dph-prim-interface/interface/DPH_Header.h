{-# LANGUAGE MagicHash #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted (
  -- * Types
  Elt,  Array,
  
  -- * Array Constructors
  empty,
  (+:+),     append_s,
  replicate, replicate_s, replicate_rs,
  repeat,
  indexed,
  generate,
  indices_s,
  enumFromTo,
  enumFromThenTo,
  enumFromStepLen,
  enumFromStepLenEach,

  -- * Projections
  length,
  (!:),     unsafeIndex,
  extract,
  unsafeExtracts_nss,
  unsafeExtracts_ass,
  unsafeExtracts_avs,
  drop,
  
  -- * Update
  update,

  -- * Permutation
  permute,
  bpermute,
  mbpermute,
  bpermuteDft,
      
  -- * Zipping and Unzipping
  zip,   zip3,
  unzip, unzip3,
  fsts,  snds,

  -- * Map and ZipWith
  map, zipWith, zipWith3, zipWith4,

  -- * Folds and Scans
  fold,  fold_s,  fold_ss,  fold_r,  
  fold1, fold1_s, fold1_ss,
  sum,   sum_s,   sum_ss,    sum_r,  
  count, count_s, count_ss,
  and, 
  scan,

  -- * Pack and Filter
  pack,
  packByTag,
  filter,
  pick,
  
  -- * Combine and Interleave
  combine, combine2,
  interleave,

  -- * Selectors
  Sel2,
  mkSel2, 
  tagsSel2,
  indicesSel2,
  elementsSel2_0,
  elementsSel2_1,
  repSel2,
  tagsToSel2,
  
  SelRep2,
  mkSelRep2,
  indicesSelRep2,
  elementsSelRep2_0,
  elementsSelRep2_1,
    
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
  promoteSegdToSSegd,
  isContiguousSSegd,
  lengthOfSSegd,
  lengthsOfSSegd,
  indicesOfSSegd,
  startsOfSSegd,
  sourcesOfSSegd,
  getSegOfSSegd,
  appendSSegd,
  
  -- * Virtual Segment Descriptors
  VSegd,
  mkVSegd,
  validVSegd,
  emptyVSegd,
  singletonVSegd,
  promoteSegdToVSegd,
  promoteSSegdToVSegd,
  isManifestVSegd,
  isContiguousVSegd,
  lengthOfVSegd,
  takeVSegidsOfVSegd,
  takeVSegidsRedundantOfVSegd,
  takeSSegdOfVSegd,
  takeSSegdRedundantOfVSegd,
  takeLengthsOfVSegd,
  getSegOfVSegd,
  demoteToSSegdOfVSegd,
  unsafeDemoteToSegdOfVSegd,
  updateVSegsOfVSegd,
  updateVSegsReachableOfVSegd,
  appendVSegd,
  combine2VSegd,
  

  -- * Irregular two dimensional arrays.
  Elts, Arrays,
  emptys,
  singletons,
  lengths,
  unsafeIndexs,
  unsafeIndex2s,
  appends,
  fromVectors,
  toVectors,
  
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
