{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification #-}

module Data.Array.Parallel.PArray.PDataBase where
import Data.Array.Parallel.PArray.PData


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

