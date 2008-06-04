module Data.Array.Parallel.Unlifted (
  UA, UArr, SUArr, USegd,

  length,
  empty, replicate, replicateEach, repeat, (+:+),
  (!:), bpermute,
  pack, combine,
  enumFromTo, enumFromToEach, indexed,
  
  zip, zip3, unzip, unzip3, fsts,
  map, zipWith, zipWith3,

  fold, fold1, sum, scan,

  (>:), concat, (^+:+^), length_s, lengths_s, indices_s,
  indexed_s,
  fold_s, fold1_s,
  toUSegd
) where

import Data.Array.Parallel.Base   ( (:*:) )
import Prelude                    (Num, Int, Bool)
