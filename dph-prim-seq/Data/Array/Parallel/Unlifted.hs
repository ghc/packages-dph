{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- | Primitive sequential combinators that work on flat, unlifted arrays.
--
--   This set of combinators is used when the program is compiled with @-fdph-seq@.
--   When compiling with @-fdph-par@, the ones in the @dph-prim-par package@ are used
--   instead. The @dph-prim-par package@ exports the same names, but all combinators
--   are implemented sequentially.
--
--   The API is defined in @DPH_Header.h@ and @DPH_Interface.h@ to ensure that both
--   @dph-prim-par@ and @dph-prim-seq@ really do export the same symbols.

#include "DPH_Header.h"

import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.Basics
import Data.Array.Parallel.Unlifted.Sequential.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Sums
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd as UVSegd

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
replicate               = V.replicate
repeat n _              = V.repeat n
indexed                 = V.indexed
enumFromTo              = V.enumFromTo
enumFromThenTo          = V.enumFromThenTo
enumFromStepLen         = V.enumFromStepLen
enumFromStepLenEach     = V.enumFromStepLenEach


-- Projections ----------------------------------------------------------------
length                  = V.length
(!:)                    = (V.!)
extract                 = V.extract
drop                    = V.drop
filter                  = V.filter
permute                 = V.permute
bpermute                = V.bpermute
mbpermute               = V.mbpermute
bpermuteDft             = V.bpermuteDft


-- Update ---------------------------------------------------------------------
update                  = V.update


-- Packing and Combining ------------------------------------------------------
pack                    = V.pack
combine                 = V.combine
combine2 tags _         = V.combine2ByTag tags
interleave              = V.interleave


-- Map and ZipWith ------------------------------------------------------------
map                     = V.map
zipWith                 = V.zipWith


-- Zipping and Unzipping ------------------------------------------------------
zip                     = V.zip
unzip                   = V.unzip
fsts                    = V.fsts
snds                    = V.snds

zip3                    = V.zip3
unzip3                  = V.unzip3


-- Folds ----------------------------------------------------------------------
fold                    = V.fold
fold1                   = V.fold1
and                     = V.and
sum                     = V.sum
scan                    = V.scan

-- Segmented Constructors ----------------------------------------------------
append_s _              = appendSU
replicate_s             = replicateSU
replicate_rs            = replicateRSU


-- Segmented Projections -----------------------------------------------------
indices_s               = indicesSU


-- Segmented Folds -----------------------------------------------------------
fold_s                  = foldSU
fold1_s                 = fold1SU
fold_r                  = foldlRU
sum_r                   = sumRU


-- Scattered Segmented Folds --------------------------------------------------
fold_ss                 = foldSSU
fold1_ss                = fold1SSU


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
isContiguousSSegd       = USSegd.isContiguous
promoteSegdToSSegd      = USSegd.fromUSegd
lengthSSegd             = USSegd.length
lengthsSSegd            = USSegd.takeLengths
indicesSSegd            = USSegd.takeIndices
startsSSegd             = USSegd.takeStarts
sourcesSSegd            = USSegd.takeSources
getSegOfSSegd           = USSegd.getSeg
appendSSegd             = USSegd.append


-- Virtual Segment Descriptors ------------------------------------------------
type VSegd              = UVSegd.UVSegd
mkVSegd                 = UVSegd.mkUVSegd
validVSegd              = UVSegd.valid
promoteSegdToVSegd      = UVSegd.fromUSegd
promoteSSegdToVSegd     = UVSegd.fromUSSegd
isManifestVSegd         = UVSegd.isManifest
isContiguousVSegd       = UVSegd.isContiguous
emptyVSegd              = UVSegd.empty
singletonVSegd          = UVSegd.singleton
lengthOfVSegd           = UVSegd.length
takeVSegidsOfVSegd      = UVSegd.takeVSegids
takeSSegdOfVSegd        = UVSegd.takeUSSegd
takeLengthsOfVSegd      = UVSegd.takeLengths
getSegOfVSegd           = UVSegd.getSeg
demoteToSSegdOfVSegd    = UVSegd.toUSSegd
demoteToSegdOfVSegd     = UVSegd.unsafeMaterialize
updateVSegsOfVSegd      = UVSegd.updateVSegs
appendVSegd             = UVSegd.append
combine2VSegd           = UVSegd.combine2


-- Selectors ------------------------------------------------------------------
type Sel2               = USel2
mkSel2 tags idxs n0 n1 _ = mkUSel2 tags idxs n0 n1
tagsSel2                = tagsUSel2
indicesSel2             = indicesUSel2
elementsSel2_0          = elementsUSel2_0
elementsSel2_1          = elementsUSel2_1
repSel2 _               = ()


-- Selector Representations ---------------------------------------------------
type SelRep2             = ()
mkSelRep2 _              = ()
indicesSelRep2 tags _    = tagsToIndices2 tags
elementsSelRep2_0 tags _ = count tags 0
elementsSelRep2_1 tags _ = count tags 1


-- Random Arrays --------------------------------------------------------------
randoms                 = V.random
randomRs                = V.randomR


-- Array IO -------------------------------------------------------------------
class V.UIO a => IOElt a
hPut                    = V.hPut
hGet                    = V.hGet

toList                  = V.toList
fromList                = V.fromList

