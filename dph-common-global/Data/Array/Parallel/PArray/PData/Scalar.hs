{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification #-}
module Data.Array.Parallel.PArray.PDataScalar where

import Data.Array.Parallel.PArray.PData
import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace


-- Int ------------------------------------------------------------------------
data instance PData Sized Int	
	= PIntS (U.Array Int)

data instance PData Global Int	
	= PIntG Int

deriving instance Show (PData Sized Int)


instance PS Int where
  emptyPS
        = PIntS U.empty

  appPS (PIntS arr1) (PIntS arr2)
	= PIntS (arr1 U.+:+ arr2)

  fromListPS xx
	= PIntS (U.fromList xx)


instance PJ Sized Int where
  restrictPJ n (PIntS vec)	
	= PIntS vec

  indexPJ (PIntS vec) ix
	= vec U.!: ix


instance PJ Global Int where
  restrictPJ n (PIntG x)	
	= trace ("{- restrictPJ[Int] " ++ show n ++ " " ++ show x ++ " -}")
	$ PIntS (U.replicate n x)

  indexPJ (PIntG x) _
	= x


instance PE Int where
  repeatPE x	= PIntG x 


instance PR Int


