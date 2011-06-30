{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification #-}
module Data.Array.Parallel.PArray.PDataInstances where

import Data.Array.Parallel.PArray.PData
import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace

-- () -------------------------------------------------------------------------
data instance PData m ()	= PUnit

instance PR () where
  emptyPR	 = PUnit
  appPR _ _	 = PUnit
  fromListPR xx	 = PUnit

instance PJ m () where
  restrictPJ _ _ = PUnit
  indexPJ _ _	 = ()

instance PE () where
  repeatPE _	 = PUnit

instance PM ()

-- Int ------------------------------------------------------------------------
data instance PData Sized Int	
	= PIntS (U.Array Int)

data instance PData Global Int	
	= PIntG Int

deriving instance Show (PData Sized Int)

instance PR Int where
  emptyPR       = PIntS U.empty

  appPR (PIntS arr1) (PIntS arr2)
	= PIntS (arr1 U.+:+ arr2)

  fromListPR xx
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

instance PM Int


-- PData Sized (PData m a) ----------------------------------------------------
data instance PData Sized  (PArray a)
	= PNestedS U.Segd (PData Sized a)

data instance PData Global (PArray a)
	= PNestedG (PArray a)

instance PR a => PR (PArray a) where
  emptyPR
	= PNestedS (U.mkSegd U.empty U.empty 0) emptyPR

  appPR (PNestedS segd1 d1) (PNestedS segd2 d2) 
   	= undefined

  fromListPR xx
	= undefined


instance PJ Sized a => PJ Sized (PArray a) where 
  indexlPJ n (PNestedS segd d1) d2
   = let d1s  = restrictPJ n d1
         d2s  = restrictPJ n d2
     in  undefined -- zipWithSegdSized indexPJ segd d1s d2s

instance PM a => PJ Global (PArray a) where

  indexlPJ n (PNestedG (PArray _ d1)) d2
   = let PIntS vec2  = restrictPJ n d2
     in  PArray (error "indexlPJ: fake array size, not used by caller")
		(fromListPR (map (indexPJ d1) $ U.toList vec2))

instance PE a => PE (PArray a) where
  repeatPE x = PNestedG x

instance PM a => PM (PArray a)

zipWithSegdSized
	:: (PData Sized a -> b -> c) 
	-> U.Segd -> PData Sized a 
	-> PData Sized b
	-> PData Sized c
zipWithSegdSized = undefined



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
