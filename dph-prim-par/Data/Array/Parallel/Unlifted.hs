{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Distributed ( DT )
import Data.Array.Parallel.Unlifted.Sequential
  hiding ((!:), (+:+))
import qualified Data.Array.Parallel.Unlifted.Sequential
  as U

#include "DPH_Interface.h"

class (UA a, DT a) => Elt a
type Array = UArr
type Segd = USegd

length = lengthU
empty = emptyU
replicate = replicateUP
repeat n _ = repeatUP n
(!:) = (U.!:)
extract = extractU
drop = dropUP
permute = permuteU
bpermuteDft = bpermuteDftU
bpermute = bpermuteUP
mbpermute = mbpermuteU
update = updateUP
(+:+) = (U.+:+)
pack = packUP
combine = combineUP
map = mapUP
filter = filterUP
zip = zipU
unzip = unzipU
fsts = fstU
snds = sndU
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
enumFromStepLenEach =enumFromStepLenEachUP

replicate_s = replicateSUP
replicate_rs = replicateRSUP
append_s = U.appendSU
fold_s = foldSUP
fold1_s = fold1SU
sum_r = sumRUP

lengthSegd = lengthUSegd
lengthsSegd = lengthsUSegd
indicesSegd = indicesUSegd
elementsSegd = elementsUSegd
lengthsToSegd  = lengthsToUSegd
mkSegd = mkUSegd
randoms = randomU
randomRs = randomRU
class UIO a => IOElt a
hPut = hPutU
hGet = hGetU
toList = fromU
fromList = toU

