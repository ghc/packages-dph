#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted (
  (:*:)(..), Elt, Array, Segd,

  length,
  empty, replicate, repeat, (+:+),
  (!:), extract, drop, permute, mbpermute, bpermute, bpermuteDft, update,
  pack, combine,
  enumFromTo, enumFromThenTo, enumFromToEach, enumFromStepLen, enumFromStepLenEach,
  indexed,
  zip, zip3, unzip, unzip3, fsts, snds,
  map, zipWith, zipWith3,
  filter,

  fold, fold1, and, sum, scan,

  replicate_s, replicate_rs, append_s,

  repeat_c,

  fold_s, fold1_s, sum_s, indices_s, sum_r,
  lengthSegd, lengthsSegd, indicesSegd, elementsSegd, lengthsToSegd, mkSegd,

  selectorToIndices2, pick, count,

  randoms, randomRs, IOElt, hGet, hPut,

  toList, fromList,
) where

import Data.Array.Parallel.Base   ( (:*:)(..) )
import Prelude                    (Num, Int, Bool, Double)
import System.IO                  (IO, Handle)
import Data.Word                  (Word8)
import qualified System.Random
import qualified Prelude

