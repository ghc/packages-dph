{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Sequential
  hiding ((!:), (+:+))
import qualified Data.Array.Parallel.Unlifted.Sequential
  as U

#include "DPH_Interface.h"

class UA a => Elt a
type Array = UArr
type Segd = USegd

length = lengthU
empty = emptyU
replicate = replicateU
repeat n _ = repeatU n
(!:) = (U.!:)
extract = extractU
drop = dropU
permute = permuteU
bpermute = bpermuteU
mbpermute = mbpermuteU
bpermuteDft = bpermuteDftU
update = updateU
(+:+) = (U.+:+)
pack = packU
combine = combineU
combine2ByTag = combine2ByTagU
map = mapU
filter = filterU
zip = zipU
unzip = unzipU
fsts = fstU
snds = sndU
zipWith = zipWithU
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
enumFromStepLenEach = enumFromStepLenEachU

replicate_s = replicateSU
replicate_rs = replicateRSU
append_s _ = appendSU
fold_s = foldSU
fold1_s = fold1SU
fold_r = foldlRU
sum_r = sumRU

indices_s = indicesSU

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

