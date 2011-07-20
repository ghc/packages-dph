{-# LANGUAGE
        TypeFamilies, MultiParamTypeClasses,
        FlexibleInstances #-}

module Data.Array.Parallel.PArray.PData.Unit where
import Data.Array.Parallel.PArray.PData.Base


-- () -------------------------------------------------------------------------
data instance PData m ()	= PUnit

instance PS () where
  {-# INLINE_PDATA emptyPS #-}
  emptyPS	        = PUnit

  {-# INLINE_PDATA appPS #-}
  appPS _ _	        = PUnit

  {-# INLINE_PDATA fromListPS #-}
  fromListPS xx	        = PUnit

instance PJ m () where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ _ _        = PUnit

  {-# INLINE_PDATA indexPJ #-}
  indexPJ _ _	        = ()

  {-# INLINE_PDATA indexlPJ #-}
  indexlPJ              = error "indexlPJ@(): undefined"

instance PE () where
  {-# INLINE_PDATA repeatPE #-}
  repeatPE _	        = PUnit

instance PR ()
