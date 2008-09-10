module Data.Array.Parallel.Unlifted (
  Elt, Array, SArray, Segd,

  length,
  empty, replicate, replicateEach, repeat, (+:+),
  (!:), bpermute,
  pack, combine,
  enumFromTo, enumFromToEach, indexed,
  
  zip, zip3, unzip, unzip3, fsts,
  map, zipWith, zipWith3,

  fold, fold1, sum, scan,

  randoms, randomRs,

  (>:), concat, (^+:+^), length_s, lengths_s, indices_s,
  indexed_s,
  fold_s, fold1_s, sum_s,
  toSegd
) where

import Data.Array.Parallel.Base   ( (:*:) )
import Prelude                    (Num, Int, Bool, Double)
import qualified System.Random

