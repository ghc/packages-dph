{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Distributed ( DT )
import Data.Array.Parallel.Unlifted.Sequential
  hiding ((!:), (+:+), (>:), (^+:+^), toUSegd)
import qualified Data.Array.Parallel.Unlifted.Sequential
  as U

#include "DPH_Interface.h"

class (UA a, DT a) => Elt a
type Array = UArr
type SArray = SUArr
type Segd = USegd

length = lengthU
empty = emptyU
replicate = replicateUP
replicateEach = replicateEachU
repeat = repeatU
(!:) = (U.!:)
bpermute = bpermuteUP
(+:+) = (U.+:+)
pack = packUP
combine = combineU
map = mapUP
zip = zipU
unzip = unzipU
fsts = fstU
zip3 = zip3U
unzip3 = unzip3U
zipWith = zipWithUP
zipWith3 = zipWith3U
fold = foldUP
fold1 = fold1U
sum = sumUP
scan = scanUP
indexed = indexedU
enumFromTo = enumFromToUP
enumFromToEach = enumFromToEachU
concat = concatSU
(>:) = (U.>:)
(^+:+^) = (U.^+:+^)
length_s = lengthSU
lengths_s = lengthsSU
indices_s = indicesSU
fold_s = foldSUP
fold1_s = fold1SU
sum_s = sumSUP
indexed_s = indexedSUP
toSegd = U.toUSegd

