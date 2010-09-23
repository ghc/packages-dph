{-# LANGUAGE CPP #-}

#include "DPH_Header.h"

import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.Segmented

#include "DPH_Interface.h"

class V.Unbox a => Elt a
type Array = V.Vector
type Segd = USegd
type Sel2 = USel2
type SelRep2 = ()

length = V.length
empty = V.empty
replicate = V.replicate
repeat n _ = V.repeat n
(!:) = (V.!)
extract = V.extract
drop = V.drop
permute = V.permute
bpermute = V.bpermute
mbpermute = V.mbpermute
bpermuteDft = V.bpermuteDft
update = V.update
(+:+) = (V.++)
interleave = V.interleave
pack = V.pack
combine = V.combine
combine2 tags _ = V.combine2ByTag tags
map = V.map
filter = V.filter
zip = V.zip
unzip = V.unzip
fsts = V.fsts
snds = V.snds
zipWith = V.zipWith
fold = V.fold
fold1 = V.fold1
and = V.and
sum = V.sum
scan = V.scan
indexed = V.indexed
enumFromTo = V.enumFromTo
enumFromThenTo = V.enumFromThenTo
enumFromStepLen = V.enumFromStepLen
enumFromStepLenEach = V.enumFromStepLenEach

mkSel2 tags idxs n0 n1 _ = mkUSel2 tags idxs n0 n1
tagsSel2 = tagsUSel2
indicesSel2 = indicesUSel2
elementsSel2_0 = elementsUSel2_0
elementsSel2_1 = elementsUSel2_1
repSel2 _ = ()

mkSelRep2 tags = ()
indicesSelRep2 tags _ = tagsToIndices2 tags
elementsSelRep2_0 tags _ = count tags 0
elementsSelRep2_1 tags _ = count tags 1

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
randoms = V.random
randomRs = V.randomR
class V.UIO a => IOElt a
hPut = V.hPut
hGet = V.hGet
toList = V.toList
fromList = V.fromList

