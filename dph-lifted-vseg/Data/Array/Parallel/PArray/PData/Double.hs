{-# LANGUAGE
        CPP,
        BangPatterns,
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        MultiParamTypeClasses,
        StandaloneDeriving,
        ExistentialQuantification #-}

#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Double where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import Text.PrettyPrint

data instance PData Double
        = PDouble !(U.Array Double)

deriving instance Show (PData Double)


instance PprPhysical (PData Double) where
  pprp (PDouble vec)
   =   text "PDouble"
   <+> text (show $ U.toList vec)


instance PprVirtual (PData Double) where
  pprv (PDouble vec)
   = text (show $ U.toList vec)


instance PR Double where
  {-# INLINE_PDATA validPR #-}
  validPR _
        = True

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PDouble U.empty

  {-# INLINE_PDATA nfPR #-}
  nfPR (PDouble xx)
        = xx `seq` ()

  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PDouble xx)
        = U.length xx

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
        = PDouble (U.replicate len x)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PDouble arr)
        = PDouble (U.replicate_s segd arr)
                
  {-# INLINE_PDATA indexPR #-}
  indexPR (PDouble arr) ix
        = arr `VU.unsafeIndex` ix

  {-# INLINE_PDATA indexlPR #-}
  indexlPR _ arr@(PNested vsegd psegdatas) (PInt ixs)
   = PDouble $ U.zipWith get (pnested_vsegids arr) ixs
   where
         -- Unbox these vectors outside the get loop.
         !psegsrcids    = U.takeVSegidsOfVSegd vsegd
         !psegstarts    = U.startsSSegd $ U.takeSSegdOfVSegd vsegd
         !psegvecs      = V.map (\(PDouble vec) -> vec) psegdatas

         -- Lookup a single element from a virtual segment.
         get !vsegid !ix
          = let !psegsrcid       = psegsrcids `VU.unsafeIndex` vsegid
                !psegvec         = psegvecs   `V.unsafeIndex`  psegsrcid
                !psegstart       = psegstarts `VU.unsafeIndex` vsegid
                !elemIx          = psegstart + ix
                !elemVal         = psegvec `VU.unsafeIndex` elemIx
            in  elemVal

  {-# INLINE_PDATA extractPR #-}
  extractPR (PDouble arr) start len 
        = PDouble (U.extract arr start len)

  {-# INLINE_PDATA extractsPR #-}
  extractsPR arrs ussegd
   = let segsrcs        = U.sourcesSSegd ussegd
         segstarts      = U.startsSSegd  ussegd
         seglens        = U.lengthsSSegd ussegd
     in  PDouble (uextracts (V.map (\(PDouble arr) -> arr) arrs)
                        segsrcs segstarts seglens)
                
  {-# INLINE_PDATA appendPR #-}
  appendPR (PDouble arr1) (PDouble arr2)
        = PDouble (arr1 U.+:+ arr2)

  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PDouble arr1) segd2 (PDouble arr2)
        = PDouble $ U.append_s segdResult segd1 arr1 segd2 arr2

  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PDouble arr1) arrTags tag
        = PDouble (U.packByTag arr1 arrTags tag)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PDouble arr1) (PDouble arr2)
        = PDouble (U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2)

  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR xx
        = PDouble (U.fromList $ V.toList xx)

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR (PDouble arr)
        = V.fromList $ U.toList arr

  {-# INLINE_PDATA fromUArrayPR #-}
  fromUArrayPR xx
        = PDouble xx

  {-# INLINE_PDATA toUArrayPR #-}
  toUArrayPR (PDouble xx)
        = xx
