{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification #-}
module Data.Array.Parallel.PArray.PData.Scalar where
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace


-- Int ------------------------------------------------------------------------
data instance PData Sized Int	
	= PIntS (U.Array Int)

data instance PData Global Int	
	= PIntG Int

deriving instance Show (PData Sized  Int)
deriving instance Show (PData Global Int)

instance PS Int where
  emptyPS
        = PIntS U.empty

  appPS (PIntS arr1) (PIntS arr2)
	= PIntS (arr1 U.+:+ arr2)

  constructPS f ixs
        = PIntS (U.map f ixs)

  fromListPS xx
	= PIntS (U.fromList xx)

  fromUArrayPS xx
        = PIntS xx


instance PJ Sized Int where
  restrictPJ n (PIntS vec)	
	= PIntS vec

  indexPJ (PIntS vec) ix
	= vec U.!: ix


instance PJ Global Int where
  restrictPJ n (PIntG x)	
	= trace ("{- restrictPJ@Int " ++ show n ++ " " ++ show x ++ " -}")
	$ PIntS (U.replicate n x)

  indexPJ (PIntG x) _
	= x

instance PE Int where
  repeatPE x	= PIntG x 


instance PR Int


-- Double ---------------------------------------------------------------------
data instance PData Sized Double
        = PDoubleS (U.Array Double)
        
data instance PData Global Double
        = PDoubleG Double
        
deriving instance Show (PData Sized  Double)
deriving instance Show (PData Global Double)

instance PS Double where
  emptyPS
        = PDoubleS U.empty

  appPS (PDoubleS arr1) (PDoubleS arr2)
	= PDoubleS (arr1 U.+:+ arr2)

  constructPS f ixs
        = PDoubleS (U.map f ixs)

  fromListPS xx
	= PDoubleS (U.fromList xx)

  fromUArrayPS xx
        = PDoubleS xx


instance PJ Sized Double where
  restrictPJ n (PDoubleS vec)	
	= PDoubleS vec

  indexPJ (PDoubleS vec) ix
	= vec U.!: ix


instance PJ Global Double where
  restrictPJ n (PDoubleG x)	
	= trace ("{- restrictPJ@Double " ++ show n ++ " " ++ show x ++ " -}")
	$ PDoubleS (U.replicate n x)

  indexPJ (PDoubleG x) _
	= x

instance PE Double where
  repeatPE x	= PDoubleG x 


instance PR Double

