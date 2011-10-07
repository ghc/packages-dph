{-# LANGUAGE
        CPP,
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
import Text.PrettyPrint

data instance PData Double
        = PDouble (U.Array Double)

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
  replicatesPR lens (PDouble arr)
        = PDouble (U.replicate_s (U.lengthsToSegd lens) arr)
                
  {-# INLINE_PDATA indexPR #-}
  indexPR (PDouble arr) ix
        = arr U.!: ix

  {-# INLINE_PDATA indexlPR #-}
  indexlPR _ arr (PInt ixs)
        = PDouble
        $ U.zipWith (\vsegid ix 
                        -> ((pnested_psegdata arr) V.! ((pnested_psegsrcids arr)  U.!: vsegid)) 
                                   `indexPR` ((pnested_psegstarts arr) U.!: vsegid + ix))
                    (pnested_vsegids arr) ixs

  {-# INLINE_PDATA extractPR #-}
  extractPR (PDouble arr) start len 
        = PDouble (U.extract arr start len)

  {-# INLINE_PDATA extractsPR #-}
  extractsPR arrs srcids ixsBase lens
        = PDouble (uextracts (V.map (\(PDouble arr) -> arr) arrs)
                        srcids ixsBase lens)
                
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
