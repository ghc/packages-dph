{-# LANGUAGE MagicHash #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted (
  -- * Basics
  Elt, Array,  
  length,
  
  -- * Constructors
  empty,
  (+:+),
  generate,
  replicate, repeat,
  indexed,
  enumFromTo, enumFromThenTo, enumFromStepLen, enumFromStepLenEach,

  -- * Projections
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

  -- * Zips and ZipWith
  zip,
  unzip, fsts, snds,
  map, zipWith, zipWith3,
  
  -- * Folds
  fold, fold1,
  and, sum, scan,


  -- * Segmented Constructors
  append_s, replicate_s, replicate_rs, 

  -- * Segmented Folds
  fold_s, fold1_s, fold_r, sum_s,  sum_r,
  
  -- * Segment Descriptors
  Segd,
  indices_s,
  lengthSegd, lengthsSegd, indicesSegd, elementsSegd, lengthsToSegd,
  mkSegd, plusSegd,

  -- * Selectors
  Sel2,
  mkSel2, 
  tagsSel2, indicesSel2, elementsSel2_0, elementsSel2_1, repSel2,
  tagsToSel2,
  
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

