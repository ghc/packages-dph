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
  {-# INLINE_PDATA emptyPS #-}
  emptyPS
        = PIntS U.empty

  {-# INLINE_PDATA appPS #-}
  appPS (PIntS arr1) (PIntS arr2)
	= PIntS (arr1 U.+:+ arr2)

  {-# INLINE_PDATA constructPS #-}
  constructPS f ixs
        = PIntS (U.map f ixs)

  {-# INLINE_PDATA nfPS #-}
  nfPS (PIntS xx)
        = xx `seq` ()

  {-# INLINE_PDATA fromListPS #-}
  fromListPS xx
	= PIntS (U.fromList xx)

  {-# INLINE_PDATA fromUArrayPS #-}
  fromUArrayPS xx
        = PIntS xx

  {-# INLINE_PDATA toUArrayPS #-}
  toUArrayPS (PIntS xx)
        = xx


instance PJ Sized Int where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n (PIntS vec)	
	= PIntS vec

  {-# INLINE_PDATA indexPJ #-}
  indexPJ (PIntS vec) ix
	= vec U.!: ix


instance PJ Global Int where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n (PIntG x)	
	= trace ("{- restrictPJ@Int " ++ show n ++ " " ++ show x ++ " -}")
	$ PIntS (U.replicate n x)

  {-# INLINE_PDATA indexPJ #-}
  indexPJ (PIntG x) _
	= x

instance PE Int where
  {-# INLINE_PDATA repeatPE #-}
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
  {-# INLINE_PDATA emptyPS #-}
  emptyPS
        = PDoubleS U.empty

  {-# INLINE_PDATA appPS #-}
  appPS (PDoubleS arr1) (PDoubleS arr2)
	= PDoubleS (arr1 U.+:+ arr2)

  {-# INLINE_PDATA constructPS #-}
  constructPS f ixs
        = PDoubleS (U.map f ixs)

  {-# INLINE_PDATA fromListPS #-}
  fromListPS xx
	= PDoubleS (U.fromList xx)

  {-# INLINE_PDATA nfPS #-}
  nfPS (PDoubleS xx)
        = xx `seq` ()

  {-# INLINE_PDATA fromUArrayPS #-}
  fromUArrayPS xx
        = PDoubleS xx

  {-# INLINE_PDATA toUArrayPS #-}
  toUArrayPS (PDoubleS xx)
        = xx



instance PJ Sized Double where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n (PDoubleS vec)	
	= PDoubleS vec

  {-# INLINE_PDATA indexPJ #-}
  indexPJ (PDoubleS vec) ix
	= vec U.!: ix


instance PJ Global Double where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n (PDoubleG x)	
	= trace ("{- restrictPJ@Double " ++ show n ++ " " ++ show x ++ " -}")
	$ PDoubleS (U.replicate n x)

  {-# INLINE_PDATA indexPJ #-}
  indexPJ (PDoubleG x) _
	= x

instance PE Double where
  {-# INLINE_PDATA repeatPE #-}
  repeatPE x	= PDoubleG x 


instance PR Double

