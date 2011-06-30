{-# LANGUAGE
        TypeFamilies, MultiParamTypeClasses,
        FlexibleInstances #-}

module Data.Array.Parallel.PArray.PData.Unit where
import Data.Array.Parallel.PArray.PData.Base


-- () -------------------------------------------------------------------------
data instance PData m ()	= PUnit

instance PS () where
  emptyPS	        = PUnit
  appPS _ _	        = PUnit
  fromListPS xx	        = PUnit

instance PJ m () where
  restrictPJ _ _        = PUnit
  indexPJ _ _	        = ()
  indexlPJ              = error "indexlPJ@(): undefined"

instance PE () where
  repeatPE _	        = PUnit

instance PR ()
