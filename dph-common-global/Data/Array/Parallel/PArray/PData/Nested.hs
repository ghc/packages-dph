
{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification,
	UndecidableInstances #-}

module Data.Array.Parallel.PArray.PData.Nested where
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Base

import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace
import Text.PrettyPrint


instance Show U.Segd where
        show _  = "SEGD"

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
  =   lbrack <> hcat (punctuate comma (map pprv $ toListPR arr)) <> rbrack

      
deriving instance Show (PData a) 
        => Show (PData (PArray a))


instance PR a => PR (PArray a) where
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
  extractPR (PNested vsegids pseglens psegstarts psegsrcs psegdata) ix len
   = PNested    (U.extract vsegids ix len)
                pseglens
                psegstarts
                psegsrcs
                psegdata

  extractsPR getArr srcids seglens segstarts 
   = error "extractsPR[PArray]: not done yet"

  {-# INLINE_PDATA appPR #-}
  appPR (PNested vsegids1 pseglens1 psegstarts1 psegsrcs1 psegdata1)
        (PNested vsegids2 pseglens2 psegstarts2 psegsrcs2 psegdata2)
   = PNested    (vsegids1    U.+:+ (U.map (+ (U.length vsegids1)) vsegids2))
                (pseglens1   U.+:+ pseglens2)
                (psegstarts1 U.+:+ psegstarts2)
                (psegsrcs1   U.+:+ (U.map (+ (V.length psegdata1)) psegsrcs2))
                (psegdata1   V.++  psegdata2)


  -- TODO: bpermuteDft isn't parallelised
  packByTagPR (PNested vsegids pseglens psegstarts psegsrcs psegdata) tags tag
   = let 
         -- Pack the vsegids to determine which of the vsegs are present in the result.
         --  eg  tags:           [0 1 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0 1 1]   tag = 1
         --      vsegids:        [0 0 1 1 2 2 2 2 3 3 4 4 4 5 5 5 5 6 6]
         --  =>  vsegids_packed: [  0 1 1         3         5   5   6 6]
         --
         vsegids_packed   
          = U.packByTag vsegids tags tag

         -- Determine which of the psegs are still reachable from the vsegs.
         -- This produces an array of flags, 
         --    with reachable   psegs corresponding to 1
         --    and  unreachable psegs corresponding to 0
         -- 
         --  eg  vsegids_packed: [0 1 1 3 5 5 6 6]
         --   => psegids_used:   [1 1 0 1 0 1 1]
         --  
         --  Note that psegids '2' and '4' are not in vsegids_packed.
         psegids_used
          = U.bpermuteDft (U.length pseglens)
                          (const 0)
                          (U.zip vsegids_packed (U.replicate (U.length vsegids_packed) 1))

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
         --  eg  vsegids_packed: [0 1 1 3 5 5 6 6]
         --      psegids_map:    [0 1 -1 2 -1 3 4]
         -- 
         --      vsegids':       [0 1 1 2 3 3 4 4]
         --
         vsegids'
          = U.map (psegids_map U.!:) vsegids_packed
         
     in  PNested 
                vsegids'
                (U.packByTag pseglens   psegids_used 1)
                (U.packByTag psegstarts psegids_used 1)
                (U.packByTag psegsrcs   psegids_used 1)
                psegdata


  -- Conversions ----------------------
  {-# INLINE_PDATA fromListPR #-}
  fromListPR xx
   = case xx of
      []      -> emptyPR
      xx@(x:xs)
       -> let segd      = U.lengthsToSegd $ U.fromList $ map lengthPA xx
          in  PNested
                { pnested_vsegids       = U.enumFromTo 0 (length xx - 1)
                , pnested_pseglens      = U.lengthsSegd segd
                , pnested_psegstarts    = U.indicesSegd segd
                , pnested_psegsrcs      = U.replicate (length xx) 0
                , pnested_psegdata      = V.singleton (foldl1 appPR $ map unpackPA xx) }

  {-# INLINE_PDATA toListPR #-}
  toListPR arr
   = map (indexPR arr) [0 .. U.length (pnested_vsegids arr) - 1]

  fromUArrayPR  = error "fromUArrayPR[PArray]: not defined yet"
   
  toUArrayPR    = error "toUArrayPR[PArray]: not defined et"



concatPR :: PR a => PData (PArray a) -> PData a
concatPR (PNested vsegids pseglens psegstarts psegsrcs psegdata) 
 = let  srcids          = U.bpermute psegsrcs   vsegids
        segstarts       = U.bpermute psegstarts vsegids
        seglens         = U.bpermute pseglens   vsegids
   in   extractsPR (psegdata V.!) srcids segstarts seglens


-- | This returns a fake length, concatPA is just for testing.
concatPA :: PR a => PArray (PArray a) -> PArray a
concatPA (PArray n darr)
        = PArray 0 (concatPR darr)

