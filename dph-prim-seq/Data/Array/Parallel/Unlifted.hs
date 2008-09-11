{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Sequential
  hiding ((!:), (+:+), (>:), (^+:+^), toUSegd)
import qualified Data.Array.Parallel.Unlifted.Sequential
  as U

#include "DPH_Interface.h"

class UA a => Elt a
type Array = UArr
type SArray = SUArr
type Segd = USegd

length = lengthU
empty = emptyU
replicate = replicateU
replicateEach = replicateEachU
repeat = repeatU
(!:) = (U.!:)
bpermute = bpermuteU
(+:+) = (U.+:+)
pack = packU
combine = combineU
map = mapU
zip = zipU
unzip = unzipU
fsts = fstU
zip3 = zip3U
unzip3 = unzip3U
zipWith = zipWithU
zipWith3 = zipWith3U
fold = foldU
fold1 = fold1U
sum = sumU
scan = scanU
indexed = indexedU
enumFromTo = enumFromToU
enumFromToEach = enumFromToEachU
randoms = randomU
randomRs = randomRU
concat = concatSU
(>:) = (U.>:)
(^+:+^) = (U.^+:+^)
length_s = lengthSU
lengths_s = lengthsSU
indices_s = indicesSU
fold_s = foldSU
fold1_s = fold1SU
sum_s = sumSU
indexed_s = indexedSU
toSegd = U.toUSegd
toList = fromU
fromList = toU

