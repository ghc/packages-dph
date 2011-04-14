{-# LANGUAGE PackageImports, CPP #-}

-- | Primitive parallel combinators that work on flat, unlifted arrays.
--   Some of them don't actually have parallel implementations, so we bail out
--   to the regular sequential ones.
--
--   This set of combinators is used when the program is comiled with -fdph-par.
--   When compiling with -fdph-seq, the ones in the dph-prim-seq package are used
--   instead. The dph-prim-seq package exports the same names, but all combinators
--   are implemented sequentially.
--
--   The API is defined in DPH_Header.h and DPH_Interface.h to ensure that both
--   dph-prim-par and dph-prim-seq really do export the same symbols.

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Distributed ( DT )
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector         as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Segmented      as Seq
import Data.Array.Parallel.Unlifted.Sequential.Vector (Unbox,Vector)

#include "DPH_Interface.h"

class (Unbox a, DT a) => Elt a

type Array              = Vector
type Segd               = UPSegd
type Sel2               = UPSel2
type SelRep2            = UPSelRep2

length                  = Seq.length
empty                   = Seq.empty
replicate               = replicateUP
repeat n _              = repeatUP n
(!:)                    = (Seq.!)
extract                 = Seq.extract
drop                    = dropUP
permute                 = Seq.permute
bpermuteDft             = Seq.bpermuteDft
bpermute                = bpermuteUP
mbpermute               = Seq.mbpermute
update                  = updateUP
(+:+)                   = (Seq.++)
interleave              = interleaveUP
pack                    = packUP
combine                 = combineUP
combine2                = combine2UP
map                     = mapUP
filter                  = filterUP
zip                     = Seq.zip
unzip                   = Seq.unzip
-- zip3                 = V.zip3
-- unzip3               = V.unzip3
fsts                    = Seq.fsts
snds                    = Seq.snds
zipWith                 = zipWithUP
fold                    = foldUP
fold1                   = Seq.fold1
and                     = andUP
sum                     = sumUP
scan                    = scanUP
indexed                 = indexedUP
enumFromTo              = enumFromToUP
enumFromThenTo          = enumFromThenToUP
enumFromStepLen         = enumFromStepLenUP
enumFromStepLenEach     = enumFromStepLenEachUP

mkSel2                  = mkUPSel2
tagsSel2                = tagsUPSel2
indicesSel2             = indicesUPSel2
elementsSel2_0          = elementsUPSel2_0
elementsSel2_1          = elementsUPSel2_1
repSel2                 = repUPSel2

mkSelRep2               = mkUPSelRep2
indicesSelRep2          = indicesUPSelRep2
elementsSelRep2_0       = elementsUPSelRep2_0
elementsSelRep2_1       = elementsUPSelRep2_1

replicate_s             = replicateSUP
replicate_rs            = replicateRSUP
append_s                = appendSUP
fold_s                  = foldSUP
fold1_s                 = fold1SUP
fold_r                  = Seq.foldlRU
sum_r                   = sumRUP

indices_s segd          = indicesSUP segd

lengthSegd              = lengthUPSegd
lengthsSegd             = lengthsUPSegd
indicesSegd             = indicesUPSegd
elementsSegd            = elementsUPSegd
mkSegd                  = mkUPSegd
randoms                 = Seq.random
randomRs                = Seq.randomR


class Seq.UIO a => IOElt a
hPut                    = Seq.hPut
hGet                    = Seq.hGet
toList                  = Seq.toList
fromList                = Seq.fromList
