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

class (Unbox a, DT a) => Elt a

type Array      = Vector
type Segd       = UPSegd
type Sel2       = UPSel2
type SelRep2    = UPSelRep2


-- Constant time operations ---------------------------------------------------
--   We don't bother tracing these.

length                  = Seq.length
empty                   = Seq.empty
zip                     = Seq.zip
unzip                   = Seq.unzip
fsts                    = Seq.fsts
snds                    = Seq.snds
(!:)                    = (Seq.!)

elementsSel2_0          = elementsUPSel2_0
elementsSel2_1          = elementsUPSel2_1
repSel2                 = repUPSel2

mkSelRep2               = mkUPSelRep2
indicesSelRep2          = indicesUPSelRep2
elementsSelRep2_0       = elementsUPSelRep2_0
elementsSelRep2_1       = elementsUPSelRep2_1

mkSegd                  = mkUPSegd
lengthSegd              = lengthUPSegd
lengthsSegd             = lengthsUPSegd
indicesSegd             = indicesUPSegd
elementsSegd            = elementsUPSegd


-------------------------------------------------------------------------------
-- These take least O(n) time in the length of the vector.
--   NOTE: That actual tracing is only enabled when 
--         dph-base/D/A/P/Config.tracePrimEnabled is set to True,
--         otherwise tracePrim is a no-op.

replicate n val 
        =  tracePrim (TraceReplicate n)
        $! replicateUP n val


repeat n _ arr
        =  tracePrim (TraceRepeat n (Seq.length arr))
        $! repeatUP n arr


extract arr i n
        =  tracePrim (TraceExtract (Seq.length arr) i n)
        $! Seq.extract arr i n


drop n arr
        =  tracePrim (TraceDrop n (Seq.length arr))
        $! dropUP n arr


permute arrSrc arrIxs
        =  tracePrim (TracePermute (Seq.length arrSrc))
        $! Seq.permute arrSrc arrIxs


bpermuteDft len f arrIxs
        =  tracePrim (TraceBPermuteDft len)
        $! Seq.bpermuteDft len f arrIxs


bpermute arrSrc arrIxs
        =  tracePrim (TraceBPermute (Seq.length arrSrc))
        $! bpermuteUP arrSrc arrIxs


mbpermute f arrSrc streamIxs
        =  tracePrim (TraceMBPermute (Seq.length arrSrc))
        $! Seq.mbpermute f arrSrc streamIxs


update arrSrc arrNew
        =  tracePrim (TraceUpdate (Seq.length arrSrc) (Seq.length arrNew))
        $! updateUP arrSrc arrNew


(+:+) arr1 arr2
        =  tracePrim (TraceAppend (Seq.length arr1 + Seq.length arr2))
        $! (Seq.++) arr1 arr2


interleave arr1 arr2
        =  tracePrim (TraceInterleave (Seq.length arr1 + Seq.length arr2))
        $! interleaveUP arr1 arr2


pack arrSrc arrFlag
        =  tracePrim (TracePack (Seq.length arrSrc))
        $! packUP arrSrc arrFlag


combine arrSel arr1 arr2
        =  tracePrim (TraceCombine (Seq.length arrSel))
        $! combineUP arrSel arr1 arr2


combine2 arrTag sel arr1 arr2
        =  tracePrim (TraceCombine2 (Seq.length arrTag))
        $! combine2UP arrTag sel arr1 arr2


map f arr
        =  tracePrim (TraceMap (Seq.length arr))
        $! mapUP f arr


filter f src
 = let  dst     = filterUP f src
   in   tracePrim (TraceFilter (Seq.length src) (Seq.length dst)) dst


zipWith f arr1 arr2
        =  tracePrim (TraceZipWith (Seq.length arr1) (Seq.length arr2))
        $! zipWithUP f arr1 arr2


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


mkSel2 tag is n0 n1 rep
        =  tracePrim (TraceMkSel2 (Seq.length is))
        $! mkUPSel2 tag is n0 n1 rep


tagsSel2 sel
 = let  tags    = tagsUPSel2 sel
   in   tracePrim (TraceTagsSel2 (Seq.length tags)) tags


indicesSel2 sel      
 = let  arr     = indicesUPSel2 sel
   in   tracePrim (TraceIndicesSel2 (Seq.length arr)) arr


replicate_s segd arr
        =  tracePrim (TraceReplicate_s (Seq.length arr))
        $! replicateSUP segd arr


replicate_rs n arr
        =  tracePrim (TraceReplicate_rs n (Seq.length arr))
        $! replicateRSUP n arr


append_s segd xd xs yd ys
 = let  arr     = appendSUP segd xd xs yd ys
   in   tracePrim (TraceAppend_s (Seq.length arr)) arr

        
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


indices_s segd
 = let  arr     = indicesSUP segd
   in   tracePrim (TraceIndices_s (Seq.length arr)) arr


-- Random arrays ------------------------------------------
randoms                 = Seq.random
randomRs                = Seq.randomR


-- IO -----------------------------------------------------
class Seq.UIO a => IOElt a
hPut                    = Seq.hPut
hGet                    = Seq.hGet
toList                  = Seq.toList
fromList                = Seq.fromList
