{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification #-}
module Data.Array.Parallel.PArray.PDataBase where

import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PDataScalar

import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace


-- () -------------------------------------------------------------------------
data instance PData m ()	= PUnit

instance PS () where
  emptyPS	 = PUnit
  appPS _ _	 = PUnit
  fromListPS xx	 = PUnit

instance PJ m () where
  restrictPJ _ _ = PUnit
  indexPJ _ _	 = ()

instance PE () where
  repeatPE _	 = PUnit

instance PR ()





{-
-- PData Sized (a, b) ---------------------------------------------------------
data instance PData Sized (a, b)
	= PTuple2 (PData Sized a) (PData Sized b)

deriving instance (Show (PData Sized a), Show (PData Sized b)) 
	=> Show (PData Sized (a, b))

instance (PR a, PR b) => PR (a, b) where
  emptyPR			
   = PTuple2 emptyPR emptyPR

  appPR (PTuple2 v11 v12) (PTuple2 v21 v22)
   = PTuple2 (v11 `appPR` v21) (v12 `appPR` v22)
	
  fromListPR xx
   = let (xs, ys)	= unzip xx
     in	 PTuple2 (fromListPR xs) (fromListPR ys)
-}
