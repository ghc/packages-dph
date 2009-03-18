{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Sequential
  hiding ((!:), (+:+), (>:), (^+:+^))
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
repeat n _ = repeatU n
(!:) = (U.!:)
extract = extractU
drop = dropU
permute = permuteU
bpermute = bpermuteU
update = updateU
(+:+) = (U.+:+)
pack = packU
combine = combineU
map = mapU
filter = filterU
zip = zipU
unzip = unzipU
fsts = fstU
zip3 = zip3U
unzip3 = unzip3U
zipWith = zipWithU
zipWith3 = zipWith3U
fold = foldU
fold1 = fold1U
and = andU
sum = sumU
scan = scanU
indexed = indexedU
enumFromTo = enumFromToU
enumFromThenTo = enumFromThenToU
enumFromStepLen = enumFromStepLenU
enumFromToEach = enumFromToEachU
randoms = randomU
randomRs = randomRU
concat = concatSU
(>:) = (U.>:)
(^+:+^) = (U.^+:+^)
length_s = lengthSU
lengths_s = lengthsSU
replicate_s = replicateSU
repeat_c = repeatCU
indices_s = indicesSU
fst_s = fstSU
snd_s = sndSU
zip_s = zipSU
bpermute_s' = bpermuteSU'
map_s = mapSU
filter_s = filterSU
pack_c = packCU
combine_c = combineCU
zipWith_s = zipWithSU
fold_s = foldSU
fold_s' = foldSU'
fold1_s = fold1SU
sum_s = sumSU
enumFromThenTo_s = enumFromThenToSU
indexed_s = indexedSU
lengthsSegd = lengthsUSegd
lengthsToSegd  = lengthsToUSegd
toSegd = toUSegd
fromSegd = fromUSegd
randoms = randomU
randomRs = randomRU
class UIO a => IOElt a
hPut = hPutU
hGet = hGetU
toList = fromU
fromList = toU
toList_s = fromSU
fromList_s = toSU

