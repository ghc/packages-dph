{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        StandaloneDeriving #-}

module Data.Array.Parallel.PArray.PData.Tuple where
import Data.Array.Parallel.PArray.PData.Base


-- PData Sized (a, b) ---------------------------------------------------------
data instance PData Sized (a, b)
	= PTuple2S (PData Sized a) (PData Sized b)

data instance PData Global (a, b)
        = PTuple2G (a, b)
        
deriving instance (Show (PData Sized a), Show (PData Sized b)) 
	=> Show (PData Sized (a, b))

deriving instance (Show a, Show b)
        => Show (PData Global (a, b))


instance (PS a, PS b) => PS (a, b) where
  emptyPS			
   = PTuple2S emptyPS emptyPS

  appPS (PTuple2S v11 v12) (PTuple2S v21 v22)
   = PTuple2S (v11 `appPS` v21) (v12 `appPS` v22)
	
  fromListPS xx
   = let (xs, ys)	= unzip xx
     in	 PTuple2S (fromListPS xs) (fromListPS ys)

