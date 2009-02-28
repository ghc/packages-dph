module Data.Array.Parallel.Unlifted (
  Elt, Array, SArray, Segd,

  length,
  empty, replicate, replicateEach, repeat, (+:+),
  (!:), drop, permute, bpermute, update,
  pack, combine,
  enumFromTo, enumFromThenTo, enumFromToEach, indexed,
  
  zip, zip3, unzip, unzip3, fsts,
  map, zipWith, zipWith3,
  filter,

  fold, fold1, and, sum, scan,

  randoms, randomRs,

  (>:), concat, (^+:+^), length_s, lengths_s, replicate_s, indices_s,
  fst_s, snd_s, zip_s,
  bpermute_s', map_s, filter_s, pack_c, combine_c, zipWith_s,
  indexed_s,
  fold_s, fold1_s, sum_s,
  enumFromThenTo_s, replicate_s,
  toSegd,

  toList, fromList
) where

import Data.Array.Parallel.Base   ( (:*:) )
import Prelude                    (Num, Int, Bool, Double)
import Data.Word                  (Word8)
import qualified System.Random

