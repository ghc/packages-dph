{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification #-}
module Data.Array.Parallel.PArray.PData.Scalar where
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Text.PrettyPrint


data instance PData Int
	= PInt (U.Array Int)

deriving instance Show (PData Int)


instance PprPhysical (PData Int) where
  pprp (PInt vec)
   =   text "PInt"
   <+> text (show $ U.toList vec)


instance PprVirtual (PData Int) where
  pprv (PInt vec)
   = text (show $ U.toList vec)


instance PR Int where
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PInt U.empty

  {-# INLINE_PDATA nfPR #-}
  nfPR (PInt xx)
        = xx `seq` ()

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
	= PInt (U.replicate len x)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PInt arr)
        = PInt (U.replicate_s (U.lengthsToSegd lens) arr)
                
  {-# INLINE_PDATA indexPR #-}
  indexPR (PInt arr) ix
	= arr U.!: ix

  {-# INLINE_PDATA extractPR #-}
  extractPR (PInt arr) start len 
        = PInt (U.extract arr start len)

  {-# INLINE_PDATA extractsPR #-}
  extractsPR arrs srcids ixsBase lens
   = PInt (uextracts (V.map (\(PInt arr) -> arr) arrs)
                     srcids ixsBase lens)
                
  {-# INLINE_PDATA appPR #-}
  appPR (PInt arr1) (PInt arr2)
	= PInt (arr1 U.+:+ arr2)

  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PInt arr1) arrTags tag
        = PInt (U.packByTag arr1 arrTags tag)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PInt arr1) (PInt arr2)
        = PInt (U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2)

  {-# INLINE_PDATA fromListPR #-}
  fromVectorPR xx
	= PInt (U.fromList $ V.toList xx)

  {-# INLINE_PDATA toListPR #-}
  toVectorPR (PInt arr)
        = V.fromList $ U.toList arr

  {-# INLINE_PDATA fromUArrayPR #-}
  fromUArrayPR xx
        = PInt xx

  {-# INLINE_PDATA toUArrayPR #-}
  toUArrayPR (PInt xx)
        = xx
