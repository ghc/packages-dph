{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- | Primitive sequential combinators that work on flat, unlifted arrays.
--
--   This set of combinators is used when the program is compiled with @-package dph-seq@.
--   When compiling with @-package dph-par@, the ones in the @dph-prim-par package@ are used
--   instead. The @dph-prim-par package@ exports the same names, but all combinators
--   are implemented sequentially.
--
--   The API is defined in @DPH_Header.h@ and @DPH_Interface.h@ to ensure that both
--   @dph-prim-par@ and @dph-prim-seq@ really do export the same symbols.

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Sequential.Vector (Unbox, Vector)
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.Basics
import Data.Array.Parallel.Unlifted.Sequential.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Sums
import Data.Array.Parallel.Unlifted.Sequential
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd as UVSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V

#include "DPH_Interface.h"

-- NOTE -----------------------------------------------------------------------
-- See DPH_Interface.h for documentation. 
-- The definitions should appear in the same order as they are defined in DPH_Interface.h


-- Basics ---------------------------------------------------------------------
class V.Unbox a => Elt a
type Array              = V.Vector


-- Constructors ---------------------------------------------------------------
empty                   = V.empty
(+:+)                   = (V.++)
append_s _              = appendSU
replicate               = V.replicate
replicate_s             = replicateSU
replicate_rs            = replicateRSU
repeat n _              = V.repeat n
indexed                 = V.indexed
indices_s               = indicesSU
enumFromTo              = V.enumFromTo
enumFromThenTo          = V.enumFromThenTo
enumFromStepLen         = V.enumFromStepLen
enumFromStepLenEach     = V.enumFromStepLenEach


-- Projections ----------------------------------------------------------------
length                  = V.length
(!:)                    = (V.!)
extract                 = V.extract
extract_ss              = extractsSU
drop                    = V.drop


-- Update ---------------------------------------------------------------------
update                  = V.update


-- Permutation ----------------------------------------------------------------
permute                 = V.permute
bpermute                = V.bpermute
mbpermute               = V.mbpermute
bpermuteDft             = V.bpermuteDft


-- Zipping and Unzipping ------------------------------------------------------
zip                     = V.zip
zip3                    = V.zip3
unzip                   = V.unzip
unzip3                  = V.unzip3
fsts                    = V.fsts
snds                    = V.snds


-- Map and ZipWith ------------------------------------------------------------
map                     = V.map
zipWith                 = V.zipWith


-- Scans and Folds ------------------------------------------------------------
scan                    = V.scan
fold                    = V.fold
fold_s                  = foldSU
fold_ss                 = foldSSU
fold_r                  = foldlRU
fold1                   = V.fold1
fold1_s                 = fold1SU
fold1_ss                = fold1SSU
sum                     = V.sum
sum_r                   = sumRU
and                     = V.and


-- Packing and Filter ---------------------------------------------------------
pack                    = V.pack
filter                  = V.filter


-- Combine and Interleave -----------------------------------------------------
combine                 = V.combine
combine2 tags _         = V.combine2ByTag tags
interleave              = V.interleave


-- Selectors ------------------------------------------------------------------
type Sel2               = USel2
mkSel2 tags idxs n0 n1 _ = mkUSel2 tags idxs n0 n1
tagsSel2                = tagsUSel2
indicesSel2             = indicesUSel2
elementsSel2_0          = elementsUSel2_0
elementsSel2_1          = elementsUSel2_1
repSel2 _               = ()

type SelRep2             = ()
mkSelRep2 _              = ()
indicesSelRep2 tags _    = tagsToIndices2 tags
elementsSelRep2_0 tags _ = count tags 0
elementsSelRep2_1 tags _ = count tags 1


-- Segment Descriptors --------------------------------------------------------
type Segd               = USegd.USegd
mkSegd                  = USegd.mkUSegd
validSegd               = USegd.valid
emptySegd               = USegd.empty
singletonSegd           = USegd.singleton
lengthSegd              = USegd.length
lengthsSegd             = USegd.takeLengths
indicesSegd             = USegd.takeIndices
elementsSegd            = USegd.takeElements


-- Slice Segment Descriptors --------------------------------------------------
type SSegd              = USSegd.USSegd
mkSSegd                 = USSegd.mkUSSegd
validSSegd              = USSegd.valid
emptySSegd              = USSegd.empty
singletonSSegd          = USSegd.singleton
promoteSegdToSSegd      = USSegd.fromUSegd
isContiguousSSegd       = USSegd.isContiguous
lengthOfSSegd           = USSegd.length
lengthsOfSSegd          = USSegd.takeLengths
indicesOfSSegd          = USSegd.takeIndices
startsOfSSegd           = USSegd.takeStarts
sourcesOfSSegd          = USSegd.takeSources
getSegOfSSegd           = USSegd.getSeg
appendSSegd             = USSegd.append


-- Virtual Segment Descriptors ------------------------------------------------
type VSegd                      = UVSegd.UVSegd
mkVSegd                         = UVSegd.mkUVSegd
validVSegd                      = UVSegd.valid
emptyVSegd                      = UVSegd.empty
singletonVSegd                  = UVSegd.singleton
promoteSegdToVSegd              = UVSegd.fromUSegd
promoteSSegdToVSegd             = UVSegd.fromUSSegd
isManifestVSegd                 = UVSegd.isManifest
isContiguousVSegd               = UVSegd.isContiguous
lengthOfVSegd                   = UVSegd.length
takeVSegidsOfVSegd              = UVSegd.takeVSegids
takeVSegidsRedundantOfVSegd     = UVSegd.takeVSegids
takeSSegdOfVSegd                = UVSegd.takeUSSegd
takeSSegdRedundantOfVSegd       = UVSegd.takeUSSegd
takeLengthsOfVSegd              = UVSegd.takeLengths
getSegOfVSegd                   = UVSegd.getSeg
demoteToSSegdOfVSegd            = UVSegd.toUSSegd
unsafeDemoteToSegdOfVSegd       = UVSegd.unsafeMaterialize
updateVSegsOfVSegd              = UVSegd.updateVSegs
updateVSegsReachableOfVSegd     = UVSegd.updateVSegsReachable
appendVSegd                     = UVSegd.append
combine2VSegd                   = UVSegd.combine2


-- Random Arrays --------------------------------------------------------------
randoms                 = V.random
randomRs                = V.randomR


-- Array IO -------------------------------------------------------------------
class V.UIO a => IOElt a
hPut                    = V.hPut
hGet                    = V.hGet

toList                  = V.toList
fromList                = V.fromList

