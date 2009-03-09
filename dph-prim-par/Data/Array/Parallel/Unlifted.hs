{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Distributed ( DT )
import Data.Array.Parallel.Unlifted.Sequential
  hiding ((!:), (+:+), (>:), (^+:+^))
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
replicateEach = replicateEachUP
repeat n _ = repeatUP n
(!:) = (U.!:)
extract = extractU
drop = dropUP
permute = permuteU
bpermute = bpermuteUP
update = updateUP
(+:+) = (U.+:+)
pack = packUP
combine = combineUP
map = mapUP
filter = filterUP
zip = zipU
unzip = unzipU
fsts = fstU
zip3 = zip3U
unzip3 = unzip3U
zipWith = zipWithUP
zipWith3 = zipWith3U
fold = foldUP
fold1 = fold1U
and = andUP
sum = sumUP
scan = scanUP
indexed = indexedUP
enumFromTo = enumFromToUP
enumFromThenTo = enumFromThenToUP
enumFromStepLen = enumFromStepLenUP
enumFromToEach = enumFromToEachU
concat = concatSU
(>:) = (U.>:)
(^+:+^) = (U.^+:+^)
length_s = lengthSU
lengths_s = lengthsSU
replicate_s = replicateSUP
repeat_c = repeatCU
indices_s = indicesSU
fst_s = fstSU
snd_s = sndSU
zip_s = zipSU
bpermute_s' = bpermuteSUP'
map_s = mapSUP
filter_s = filterSUP
pack_c = packCUP
combine_c = combineCUP
zipWith_s = zipWithSUP
fold_s = foldSUP
fold_s' = foldSUP'
fold1_s = fold1SU
sum_s = sumSUP
enumFromThenTo_s = enumFromThenToSUP
indexed_s = indexedSUP
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
