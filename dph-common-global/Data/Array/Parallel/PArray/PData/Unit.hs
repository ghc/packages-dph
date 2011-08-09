{-# LANGUAGE
        TypeFamilies, MultiParamTypeClasses,
        FlexibleInstances,
        StandaloneDeriving #-}

module Data.Array.Parallel.PArray.PData.Unit where
import Data.Array.Parallel.PArray.PData.Base


data instance PData ()
	= PUnit

deriving instance Show (PData ())

instance PR () where
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PUnit

  {-# INLINE_PDATA nfPR #-}
  nfPR xx
        = xx `seq` ()

  {-# INLINE_PDATA replicatePR #-}
  replicatePR _ _
	= PUnit

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR _ _
        = PUnit
        
  {-# INLINE_PDATA indexPR #-}
  indexPR _ _
	= ()

  {-# INLINE_PDATA extractPR #-}
  extractPR _ _ _
        = PUnit
                
  {-# INLINE_PDATA appPR #-}
  appPR _ _
	= PUnit

  {-# INLINE_PDATA fromListPR #-}
  fromListPR _
	= PUnit

  {-# INLINE_PDATA fromUArrayPR #-}
  fromUArrayPR _
        = PUnit

  {-# INLINE_PDATA toUArrayPR #-}
  toUArrayPR _
        = error "toUArrayPR: doens't work as we don't know the length"
