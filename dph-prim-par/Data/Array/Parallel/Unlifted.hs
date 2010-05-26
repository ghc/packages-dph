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
combine2ByTag = combine2ByTagUP
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
enumFromToEach = enumFromToEachUP
enumFromStepLenEach =enumFromStepLenEachUP

replicate_s = replicateSUP
replicate_rs = replicateRSUP
append_s = appendSUP
fold_s = foldSUP
fold1_s = fold1SUP
fold_r = foldlRU
sum_r = sumRUP

indices_s _ segd _ = indicesSUP segd

lengthSegd = lengthUSegd
lengthsSegd = lengthsUSegd
indicesSegd = indicesUSegd
elementsSegd = elementsUSegd
mkSegd = mkUSegd
randoms = randomU
randomRs = randomRU
class UIO a => IOElt a
hPut = hPutU
hGet = hGetU
toList = fromU
fromList = toU

