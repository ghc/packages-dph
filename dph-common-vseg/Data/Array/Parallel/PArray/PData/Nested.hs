{-# LANGUAGE
        CPP,
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification,
	UndecidableInstances #-}

#include "fusion-phases-vseg.h"

module Data.Array.Parallel.PArray.PData.Nested where
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Base

import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace
import Text.PrettyPrint


-- Nested arrays --------------------------------------------------------------
data instance PData (PArray a)
	= PNested
	{ -- Virtual segmentation. 
	  -- Defines an array of arrays based on the physical segmentation.
	  pnested_vsegids      :: U.Array Int

          -- Physical segmentation.
          -- Describes the segmentation in the underlying flat array 
	, pnested_pseglens     :: U.Array Int
	, pnested_psegstarts   :: U.Array Int
	, pnested_psegsrcs     :: U.Array Int
	, pnested_psegdata     :: V.Vector (PData a) }


-- | Pretty print the physical representation of a nested array
instance PprPhysical (PData a) => PprPhysical (PData (PArray a)) where
 pprp (PNested vsegids pseglens psegstarts psegsrcs psegdata)
  =   text "PNested"
  $+$ (nest 4 $ vcat
        [ text "vsegids:   " <+> text (show $ U.toList vsegids) 
        , text "pseglens:  " <+> text (show $ U.toList pseglens) 
        , text "psegstarts:" <+> text (show $ U.toList psegstarts) 
        , text "psegsrcs:  " <+> text (show $ U.toList psegsrcs) ]
        $$ vcat (map pprp $ V.toList psegdata)
      )


instance (PR a, PprVirtual (PData a)) => PprVirtual (PData (PArray a)) where
 pprv arr
  =   lbrack <> hcat (punctuate comma (map pprv $ V.toList $ toVectorPR arr)) <> rbrack

      
deriving instance Show (PData a) 
        => Show (PData (PArray a))

-- TODO: shift this stuff into dph-base
validIx  :: String -> Int -> Int -> Bool
validIx str len ix 
        = check str len ix (ix >= 0 && ix < len)

validLen :: String -> Int -> Int -> Bool
validLen str len ix 
        = checkLen str len ix (ix >= 0 && ix <= len)

-- TODO: slurp debug flag from base 
validBool :: String -> Bool -> Bool
validBool str b
        = if b  then True 
                else error $ "validBool check failed -- " ++ str

instance PR a => PR (PArray a) where

  -- TODO: ensure that all psegdata arrays are referenced from some psegsrc
  {-# INLINE_PDATA validPR #-}
  validPR (PNested vsegids pseglens psegstarts psegsrcs psegdata)
   = let -- The lengths of the pseglens, psegstarts and psegsrcs fields must all be the same
         fieldLensOK
                = validBool "nested field lengths"
                $ and 
                [ U.length psegstarts == U.length pseglens
                , U.length psegsrcs   == U.length pseglens ]
         
         -- Each pseg source id must point to a valid flat data array
         psegsrcsOK
                = validBool "nested psegsrcs"
                $ U.and 
                $ U.map (\srcid -> validIx "nested psegsrcs " (V.length psegdata) srcid)
                         psegsrcs

         -- Each physical segment must be a valid slice of the corresponding flat array.
         psegSlicesOK 
                = validBool "nested pseg slices"
                $ U.and 
                $ U.zipWith3 
                        (\len start srcid
                           -> let srclen = lengthPR (psegdata V.! srcid)
                              in  and [ validIx  "nested psegstart " srclen start
                                      , validLen "nested pseglen   " srclen (start + len)])
                        pseglens psegstarts psegsrcs

         -- TODO: Check that all psegs are referenced by some vseg.
         -- TODO: check all vsegs reference a valid pseg.

     in  and [ fieldLensOK
             , psegsrcsOK
             , psegSlicesOK]
                 

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
	= PNested
	{ pnested_vsegids       = U.empty
	, pnested_pseglens      = U.empty
	, pnested_psegstarts    = U.empty
	, pnested_psegsrcs      = U.empty
	, pnested_psegdata      = V.empty }


  {-# INLINE_PDATA nfPR #-}
  nfPR  = error "nfPR[PArray]: not defined yet"


  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PNested vsegids _ _ _ _)
        = U.length vsegids


  {-# INLINE_PDATA replicatePR #-}
  replicatePR c (PArray n darr)
        = PNested
        { pnested_vsegids       = U.replicate c 0
        , pnested_pseglens      = U.replicate 1 n
        , pnested_psegstarts    = U.replicate 1 0
        , pnested_psegsrcs      = U.replicate 1 0
        , pnested_psegdata      = V.singleton darr }
                

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PNested vsegids pseglens psegstarts psegsrcs psegdata)
        = PNested
        { pnested_vsegids       = U.replicate_s (U.lengthsToSegd lens) vsegids
        , pnested_pseglens      = pseglens
        , pnested_psegstarts    = psegstarts
        , pnested_psegsrcs      = psegsrcs
        , pnested_psegdata      = psegdata }


  {-# INLINE_PDATA indexPR #-}
  indexPR arr@(PNested vsegids pseglens psegstarts psegsrcs psegdatas) ix
   = let -- Get the number of the physical segment this is indexing.
         pseg       = vsegids    U.!: ix
         pseglen    = pseglens   U.!: pseg
         psegstart  = psegstarts U.!: pseg
         psegdata   = psegdatas  V.!  (psegsrcs U.!: pseg)
         
         darr       = extractPR psegdata psegstart pseglen
         
     in  PArray pseglen darr


  {-# INLINE_PDATA extractPR #-}
  -- TODO: force out unused psegs. add a quickcheck prop for this.
  extractPR (PNested vsegids pseglens psegstarts psegsrcs psegdata) start len
   = PNested    (U.extract vsegids start len)
                pseglens
                psegstarts
                psegsrcs
                psegdata


  {-# INLINE_PDATA extractsPR #-}
  extractsPR arrs segsrcs segstarts seglens 
   = let segMax         = U.sum seglens - 1
         vsegids'       = U.enumFromTo 0 segMax

         vsegids_src    = uextracts (V.map pnested_vsegids  arrs) segsrcs segstarts seglens
         srcids'        = U.replicate_s (U.lengthsToSegd seglens) segsrcs

         pseglens'      = U.zipWith (\srcid vsegid -> pnested_pseglens   (arrs V.! srcid) U.!: vsegid)
                                    srcids' vsegids_src

         psegstarts'    = U.zipWith (\srcid vsegid -> pnested_psegstarts (arrs V.! srcid) U.!: vsegid)
                                    srcids' vsegids_src

         -- As all the flat data arrays in the sources are present in the result array,
         -- we need to offset the psegsrcs field when combining multiple sources.
         -- 
         -- For example:
         --  Source Arrays:
         --   arr0  ...
         --         psrcids  :  [0, 0, 0, 1, 1]
         --         psegdata :  [PInt xs1, PInt xs2]
         --
         --   arr1  ... 
         --         psrcids  :  [0, 0, 1, 1, 2, 2, 2]
         --         psegdata :  [PInt ys1, PInt ys2, PInt ys3]
         -- 
         --   Result Array:
         --         psrcids  :  [...]
         --         psegdata :  [PInt xs1, PInt xs2, PInt ys1, PInt ys2, PInt ys3] 
         --
         --  Note that references to flatdata arrays [0, 1, 2] in arr1 need to be offset
         --  by 2 (which is length arr0.psegdata) to refer to the same flat data arrays
         --  in the result.
         -- 
         --  We encode these offsets in the psrcoffset vector:
         --       psrcoffset :  [0, 2]
         --
         psrcoffset     = V.prescanl (+) 0 $ V.map (V.length . pnested_psegdata) arrs

         psegsrcs'      = U.zipWith 
                                (\srcid vsegid 
                                        -> (pnested_psegsrcs   (arrs V.! srcid) U.!: vsegid)
                                        +  psrcoffset V.! srcid)
                                srcids' vsegids_src

         -- All flat data arrays in the sources go into the result.
         psegdata'      = V.concat $ V.toList $ V.map pnested_psegdata arrs
   
     in  PNested vsegids' pseglens' psegstarts' psegsrcs' psegdata'


  {-# INLINE_PDATA appPR #-}
  appPR (PNested vsegids1 pseglens1 psegstarts1 psegsrcs1 psegdata1)
        (PNested vsegids2 pseglens2 psegstarts2 psegsrcs2 psegdata2)
   = PNested    (vsegids1    U.+:+ (U.map (+ (U.length vsegids1)) vsegids2))
                (pseglens1   U.+:+ pseglens2)
                (psegstarts1 U.+:+ psegstarts2)
                (psegsrcs1   U.+:+ (U.map (+ (V.length psegdata1)) psegsrcs2))
                (psegdata1   V.++  psegdata2)


  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR arr tags tag
   = let -- Pack the vsegids to determine which of the vsegs are present in the result.
         --  eg  tags:           [0 1 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0 1 1]   tag = 1
         --      vsegids:        [0 0 1 1 2 2 2 2 3 3 4 4 4 5 5 5 5 6 6]
         --  =>  vsegids_packed: [  0 1 1         3         5   5   6 6]
         --
         vsegids_packed   
          = U.packByTag (pnested_vsegids arr) tags tag

     in forceSegs vsegids_packed arr


  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel2 (PNested vsegids1 pseglens1 psegstarts1 psegsrcs1 psegdata1)
                  (PNested vsegids2 pseglens2 psegstarts2 psegsrcs2 psegdata2)

   = let -- vsegids relative to combined psegs
         vsegids1'      = vsegids1
         vsegids2'      = U.map (+ (U.length vsegids1)) vsegids2

         -- psegsrcss in combined pdatas vector
         psegsrcs1'     = psegsrcs1
         psegsrcs2'     = U.map (+ (V.length psegdata1)) psegsrcs2
         
     in  PNested 
                (U.combine2 (U.tagsSel2 sel2) (U.repSel2 sel2) vsegids1' vsegids2')
                (pseglens1   U.+:+ pseglens2)
                (psegstarts1 U.+:+ psegstarts2)
                (psegsrcs1'  U.+:+ psegsrcs2')
                (psegdata1   V.++  psegdata2)


  -- Conversions ----------------------
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR xx
   | V.length xx == 0 = emptyPR
   | otherwise
   = let segd      = U.lengthsToSegd $ U.fromList $ V.toList $ V.map lengthPA xx
     in  PNested
          { pnested_vsegids       = U.enumFromTo 0 (V.length xx - 1)
          , pnested_pseglens      = U.lengthsSegd segd
          , pnested_psegstarts    = U.indicesSegd segd
          , pnested_psegsrcs      = U.replicate (V.length xx) 0
          , pnested_psegdata      = V.singleton (V.foldl1 appPR $ V.map unpackPA xx) }


  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR arr
   = V.generate (U.length (pnested_vsegids arr))
   $ indexPR arr


  fromUArrayPR  = error "fromUArrayPR[PArray]: not defined yet"   
  toUArrayPR    = error "toUArrayPR[PArray]: not defined et"


-------------------------------------------------------------------------------
concatPR :: PR a => PData (PArray a) -> PData a
concatPR (PNested vsegids pseglens psegstarts psegsrcs psegdata) 
 = let  srcids          = U.bpermute psegsrcs   vsegids
        segstarts       = U.bpermute psegstarts vsegids
        seglens         = U.bpermute pseglens   vsegids
   in   extractsPR psegdata srcids segstarts seglens


-------------------------------------------------------------------------------
-- | Impose new virtual segmentation on a nested array.
--   All physical segments that are not reachable from the virtual
--   segments are filtered out, but the underlying flat data is not touched.
--
--  TODO: bpermuteDft isn't parallelised
--
forceSegs :: U.Array Int -> PData (PArray a) -> PData (PArray a)
forceSegs vsegids_new (PNested vsegids pseglens psegstarts psegsrcs psegdata)
 = let
         -- Determine which of the psegs are still reachable from the vsegs.
         -- This produces an array of flags, 
         --    with reachable   psegs corresponding to 1
         --    and  unreachable psegs corresponding to 0
         -- 
         --  eg  vsegids_new:    [0 1 1 3 5 5 6 6]
         --   => psegids_used:   [1 1 0 1 0 1 1]
         --  
         --  Note that psegids '2' and '4' are not in vsegids_packed.
         psegids_used
          = U.bpermuteDft (U.length pseglens)
                          (const 0)
                          (U.zip vsegids_new (U.replicate (U.length vsegids_new) 1))

         -- Produce an array of used psegs.
         --  eg  psegids_used:   [1 1 0 1 0 1 1]
         --      psegids_packed: [0 1 3 5 6]
         psegids_packed
          = U.packByTag (U.enumFromTo 0 (U.length psegids_used)) psegids_used 1

         -- Produce an array that maps psegids in the source array onto
         -- psegids in the result array. If a particular pseg isn't present
         -- in the result this maps onto -1.

         --  Note that if psegids_used has 0 in some position, then psegids_map
         --  has -1 in the same position, corresponding to an unused pseg.
         
         --  eg  psegids_packed: [0 1 3 5 6]
         --                      [0 1 2 3 4]
         --      psegids_map:    [0 1 -1 2 -1 3 4]
         psegids_map
          = U.bpermuteDft (U.length pseglens)
                          (const (-1))
                          (U.zip psegids_packed (U.enumFromTo 0 (U.length psegids_packed - 1)))

         -- Use the psegids_map to rewrite the packed vsegids to point to the 
         -- corresponding psegs in the result.
         -- 
         --  eg  vsegids_new:    [0 1 1 3 5 5 6 6]
         --      psegids_map:    [0 1 -1 2 -1 3 4]
         -- 
         --      vsegids':       [0 1 1 2 3 3 4 4]
         --
         vsegids'
          = U.map (psegids_map U.!:) vsegids_new

     in  PNested 
                vsegids'
                (U.packByTag pseglens   psegids_used 1)
                (U.packByTag psegstarts psegids_used 1)
                (U.packByTag psegsrcs   psegids_used 1)
                psegdata

