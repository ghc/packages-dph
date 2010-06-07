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
type Sel2 = USel2

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
interleave = interleaveUP
pack = packUP
combine = combineUP
combine2ByTag = combine2ByTagUP
map = mapUP
filter = filterUP
zip = zipU
unzip = unzipU
fsts = fstU
snds = sndU
zipWith = zipWithUP
fold = foldUP
fold1 = fold1U
and = andUP
sum = sumUP
scan = scanUP
indexed = indexedUP
enumFromTo = enumFromToUP
enumFromThenTo = enumFromThenToUP
enumFromStepLen = enumFromStepLenUP
enumFromStepLenEach =enumFromStepLenEachUP

mkSel2 = mkUSel2
tagsSel2 = tagsUSel2
indicesSel2 = indicesUSel2
elementsSel2_0 = elementsUSel2_0
elementsSel2_1 = elementsUSel2_1

replicate_s = replicateSUP
replicate_rs = replicateRSUP
append_s = appendSUP
fold_s = foldSUP
fold1_s = fold1SUP
fold_r = foldlRU
sum_r = sumRUP

indices_s segd = indicesSUP segd

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

