
{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification,
	UndecidableInstances #-}

module Data.Array.Parallel.PArray.PData.Nested where
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Base

import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace


-- PData Sized (PData m a) ----------------------------------------------------
data instance PData Sized  (PArray a)
	= PNestedS U.Segd (PData Sized a)

data instance PData Global (PArray a)
	= PNestedG (PArray a)

instance PS a => PS (PArray a) where
  emptyPS
	= PNestedS (U.mkSegd U.empty U.empty 0) emptyPS

  appPS (PNestedS segd1 d1) (PNestedS segd2 d2) 
   	= error "appPS@PArray undefined"

  fromListPS xx
   = case xx of
      []      -> emptyPS
      xx@(x:xs)
       -> PNestedS
                (U.lengthsToSegd $ U.fromList $ map lengthPA xx)
                (foldl1 appPS $ map unpackPA xx)
                
instance PJ Sized a => PJ Sized (PArray a) where 
  restrictPJ n arr@(PNestedS segd d1)
   = arr

  indexlPJ n (PNestedS segd d1) d2
   = let d1s  = restrictPJ n d1
         d2s  = restrictPJ n d2
     in  error "indexlPJ@PArray sized undefined"
     

instance PR a => PJ Global (PArray a) where

  indexlPJ n (PNestedG (PArray _ d1)) d2
   = let PIntS vec2  = restrictPJ n d2
     in  PArray (error "indexlPJ@PArray: fake segd should not be touched by caller")
         $ constructPS (indexPJ d1) vec2


instance PE a => PE (PArray a) where
  repeatPE x = PNestedG x


instance PR a => PR (PArray a)


