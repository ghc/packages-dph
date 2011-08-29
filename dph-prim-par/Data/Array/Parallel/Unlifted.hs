{-# LANGUAGE PackageImports, CPP #-}

-- | Primitive parallel combinators that work on flat, unlifted arrays.
--   Some of them don't actually have parallel implementations, so we bail out
--   to the regular sequential ones.
--
--   This set of combinators is used when the program is comiled with @-fdph-par@.
--   When compiling with @-fdph-seq@, the ones in the @dph-prim-seq@ package are used
--   instead. The @dph-prim-seq package@ exports the same names, but all combinators
--   are implemented sequentially.
--
--   The API is defined in @DPH_Header.h@ and @DPH_Interface.h@ to ensure that both
--   @dph-prim-par@ and @dph-prim-seq@ really do export the same symbols.

#include "DPH_Header.h"

import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Base.TracePrim
import Data.Array.Parallel.Unlifted.Distributed ( DT )
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector         as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Segmented      as Seq
import Data.Array.Parallel.Unlifted.Sequential.Vector (Unbox,Vector)
import Prelude (($!))

#include "DPH_Interface.h"

-- NOTE -----------------------------------------------------------------------
-- See DPH_Interface.h for documentation. 
--
-- The definitions should appear in the same order as they are defined in DPH_Interface.h
--
-- Operations with at least O(n) time will print trace messages to console when
-- dph-base/D/A/P/Config.tracePrimEnabled is set to True.
--

-- Basics ---------------------------------------------------------------------
class (Unbox a, DT a) => Elt a
type Array      = Vector


-- Constructors ---------------------------------------------------------------
empty   = Seq.empty

(+:+) arr1 arr2
        =  tracePrim (TraceAppend (Seq.length arr1 + Seq.length arr2))
        $! (Seq.++) arr1 arr2

replicate n val 
        =  tracePrim (TraceReplicate n)
        $! replicateUP n val

repeat n _ arr
        =  tracePrim (TraceRepeat n (Seq.length arr))
        $! repeatUP n arr

indexed arr
        =  tracePrim (TraceIndexed (Seq.length arr))
        $! indexedUP arr

enumFromTo from to
 = let  arr     = enumFromToUP from to
   in   tracePrim (TraceEnumFromTo (Seq.length arr)) arr

enumFromThenTo from thn to
 = let  arr     = enumFromThenToUP from thn to
   in   tracePrim (TraceEnumFromThenTo (Seq.length arr)) arr
   
enumFromStepLen from step len
 = let  arr     = enumFromStepLenUP from step len
   in   tracePrim (TraceEnumFromStepLen (Seq.length arr)) arr

enumFromStepLenEach n starts steps lens
 = let  arr     = enumFromStepLenEachUP n starts steps lens
   in   tracePrim (TraceEnumFromStepLenEach (Seq.length arr)) arr


-- Projections ----------------------------------------------------------------
length                  = Seq.length
(!:)                    = (Seq.!)

extract arr i n
        =  tracePrim (TraceExtract (Seq.length arr) i n)
        $! Seq.extract arr i n

drop n arr
        =  tracePrim (TraceDrop n (Seq.length arr))
        $! dropUP n arr

filter f src
 = let  dst     = filterUP f src
   in   tracePrim (TraceFilter (Seq.length src) (Seq.length dst)) dst

permute arrSrc arrIxs
        =  tracePrim (TracePermute (Seq.length arrSrc))
        $! Seq.permute arrSrc arrIxs

bpermute arrSrc arrIxs
        =  tracePrim (TraceBPermute (Seq.length arrSrc))
        $! bpermuteUP arrSrc arrIxs


mbpermute f arrSrc streamIxs
        =  tracePrim (TraceMBPermute (Seq.length arrSrc))
        $! Seq.mbpermute f arrSrc streamIxs

bpermuteDft len f arrIxs
        =  tracePrim (TraceBPermuteDft len)
        $! Seq.bpermuteDft len f arrIxs


-- Update ---------------------------------------------------------------------
update arrSrc arrNew
        =  tracePrim (TraceUpdate (Seq.length arrSrc) (Seq.length arrNew))
        $! updateUP arrSrc arrNew


-- Packing and Combining ------------------------------------------------------
pack arrSrc arrFlag
        =  tracePrim (TracePack (Seq.length arrSrc))
        $! packUP arrSrc arrFlag


combine arrSel arr1 arr2
        =  tracePrim (TraceCombine (Seq.length arrSel))
        $! combineUP arrSel arr1 arr2


combine2 arrTag sel arr1 arr2
        =  tracePrim (TraceCombine2 (Seq.length arrTag))
        $! combine2UP arrTag sel arr1 arr2

interleave arr1 arr2
        =  tracePrim (TraceInterleave (Seq.length arr1 + Seq.length arr2))
        $! interleaveUP arr1 arr2


-- Map and ZipWith ------------------------------------------------------------
map f arr
        =  tracePrim (TraceMap (Seq.length arr))
        $! mapUP f arr

zipWith f arr1 arr2
        =  tracePrim (TraceZipWith (Seq.length arr1) (Seq.length arr2))
        $! zipWithUP f arr1 arr2


-- Zipping and Unzipping ------------------------------------------------------
zip     = Seq.zip
unzip   = Seq.unzip
fsts    = Seq.fsts
snds    = Seq.snds

zip3    = Seq.zip3
unzip3  = Seq.unzip3


-- Folds ----------------------------------------------------------------------
fold f x arr
        =  tracePrim (TraceFold (Seq.length arr))
        $! foldUP f x arr

        
fold1 f arr
        =  tracePrim (TraceFold1 (Seq.length arr))
        $! Seq.fold1 f arr


and arr =  tracePrim (TraceAnd (Seq.length arr))
        $! andUP arr


sum arr =  tracePrim (TraceSum (Seq.length arr))
        $! sumUP arr

                        
scan f x arr
        =  tracePrim (TraceScan (Seq.length arr))
        $! scanUP f x arr


-- Segmented Constructors -----------------------------------------------------
replicate_s segd arr
        =  tracePrim (TraceReplicate_s (Seq.length arr))
        $! replicateSUP segd arr


replicate_rs n arr
        =  tracePrim (TraceReplicate_rs n (Seq.length arr))
        $! replicateRSUP n arr


append_s segd xd xs yd ys
 = let  arr     = appendSUP segd xd xs yd ys
   in   tracePrim (TraceAppend_s (Seq.length arr)) arr


-- Segmented Projections ------------------------------------------------------
indices_s segd
 = let  arr     = indicesSUP segd
   in   tracePrim (TraceIndices_s (Seq.length arr)) arr


-- Segmented Folds ------------------------------------------------------------
fold_s f x segd arr
        =  tracePrim (TraceFold_s (Seq.length arr))
        $! foldSUP f x segd arr

        
fold1_s f segd arr
        =  tracePrim (TraceFold1_s (Seq.length arr))
        $! fold1SUP f segd arr


fold_r f z segSize arr
        =  tracePrim (TraceFold_r (Seq.length arr))
        $! Seq.foldlRU f z segSize arr


sum_r x arr
        =  tracePrim (TraceSum_r (Seq.length arr))
        $! sumRUP x arr


-- Segment Descriptors --------------------------------------------------------
type Segd               = UPSegd
mkSegd                  = mkUPSegd
lengthSegd              = lengthUPSegd
lengthsSegd             = lengthsUPSegd
indicesSegd             = indicesUPSegd
elementsSegd            = elementsUPSegd


-- Slice Segment Descriptors --------------------------------------------------
-- TODO: these point to sequential segd ops
type SSegd              = Seq.USSegd
mkSSegd                 = Seq.mkUSSegd
validSSegd              = Seq.validUSSegd
emptySSegd              = Seq.emptyUSSegd
singletonSSegd          = Seq.singletonUSSegd
promoteSegdToSSegd      = Seq.promoteUSegdToUSSegd
lengthSSegd             = Seq.lengthUSSegd
lengthsSSegd            = Seq.lengthsUSSegd
indicesSSegd            = Seq.indicesUSSegd
sourcesSSegd            = Seq.sourcesUSSegd
getSegOfSSegd           = Seq.getSegOfUSSegd
appendSSegd             = Seq.appendUSSegd


-- Virtual Segment Descriptors ------------------------------------------------
-- TODO: these point to sequential segd ops.
type VSegd              = Seq.UVSegd
mkVSegd                 = Seq.mkUVSegd
validVSegd              = Seq.validUVSegd
emptyVSegd              = Seq.emptyUVSegd
singletonVSegd          = Seq.singletonUVSegd
promoteSegdToVSegd      = Seq.promoteUSegdToUVSegd
unsafeMaterializeVSegd  = Seq.unsafeMaterializeUVSegd
promoteSSegdToVSegd     = Seq.promoteUSSegdToUVSegd
demoteVSegdToSSegd      = Seq.demoteUVSegdToUSSegd
vsegidsVSegd            = Seq.vsegidsUVSegd
ssegdVSegd              = Seq.ussegdUVSegd
lengthVSegd             = Seq.lengthUVSegd
lengthsVSegd            = Seq.lengthsUVSegd
getSegOfVSegd           = Seq.getSegOfUVSegd
updateVSegsOfVSegd      = Seq.updateVSegsOfUVSegd
appendVSegd             = Seq.appendUVSegd
combine2VSegd           = Seq.combine2UVSegd


-- Selectors ------------------------------------------------------------------
type Sel2               = UPSel2

mkSel2 tag is n0 n1 rep
        =  tracePrim (TraceMkSel2 (Seq.length is))
        $! mkUPSel2 tag is n0 n1 rep

tagsSel2 sel
 = let  tags    = tagsUPSel2 sel
   in   tracePrim (TraceTagsSel2 (Seq.length tags)) tags


indicesSel2 sel      
 = let  arr     = indicesUPSel2 sel
   in   tracePrim (TraceIndicesSel2 (Seq.length arr)) arr

elementsSel2_0          = elementsUPSel2_0
elementsSel2_1          = elementsUPSel2_1
repSel2                 = repUPSel2


-- Selector Representations ---------------------------------------------------
type SelRep2            = UPSelRep2
mkSelRep2               = mkUPSelRep2

indicesSelRep2          = indicesUPSelRep2
elementsSelRep2_0       = elementsUPSelRep2_0
elementsSelRep2_1       = elementsUPSelRep2_1


-- Random arrays --------------------------------------------------------------
randoms                 = Seq.random
randomRs                = Seq.randomR


-- IO -------------------------------------------------------------------------
class Seq.UIO a => IOElt a
hPut                    = Seq.hPut
hGet                    = Seq.hGet
toList                  = Seq.toList
fromList                = Seq.fromList
