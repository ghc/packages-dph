{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        StandaloneDeriving, ExplicitForAll,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PData.Tuple where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested

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

instance (PE a, PE b) => PE (a, b)

instance (PJ m a, PJ m b) => PJ m (a, b)

instance (PR a, PR b) => PR (a, b)


unzipPA :: PArray (a, b) -> (PArray a, PArray b)
unzipPA (PArray n (PTuple2S xs ys))
        = (PArray n xs, PArray n ys)

unzipPA_l :: forall m1 a b. PJ m1 (PArray (a, b))
          => Int -> PData m1 (PArray (a, b)) -> PData Sized (PArray a, PArray b)
unzipPA_l c vs
 = case restrictPJ c vs of
         PNestedS segd (PTuple2S xs ys)
          -> PTuple2S (PNestedS segd xs) (PNestedS segd ys)