module Data.Array.Parallel.Unlifted (
  (:*:)(..), Elt, Array, SArray, Segd,

  length,
  empty, replicate, replicateEach, repeat, (+:+),
  (!:), extract, drop, permute, bpermute, update,
  pack, combine,
  enumFromTo, enumFromThenTo, enumFromToEach, enumFromStepLen, indexed,
  
  zip, zip3, unzip, unzip3, fsts,
  map, zipWith, zipWith3,
  filter,

  fold, fold1, and, sum, scan,

  (>:), concat, (^+:+^), length_s, lengths_s, replicate_s,
  repeat_c, indices_s,
  fst_s, snd_s, zip_s,
  bpermute_s', map_s, filter_s, pack_c, combine_c, zipWith_s,
  indexed_s,
  fold_s, fold1_s, sum_s,
  enumFromThenTo_s,
  lengthsSegd, lengthsToSegd, toSegd, fromSegd,

  randoms, randomRs, IOElt, hGet, hPut,

  toList, fromList, toList_s, fromList_s
) where

import Data.Array.Parallel.Base   ( (:*:)(..) )
import Prelude                    (Num, Int, Bool, Double)
import System.IO                  (IO, Handle)
import Data.Word                  (Word8)
import qualified System.Random

