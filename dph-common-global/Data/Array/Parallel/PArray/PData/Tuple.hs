{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        StandaloneDeriving, ExplicitForAll,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PData.Tuple where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested

-- PData Sized (a, b) ---------------------------------------------------------
data instance PData m (a, b)
        = PTuple2 (PData m a) (PData m b)

deriving instance (Show (PData Sized a), Show (PData Sized b)) 
	=> Show (PData Sized (a, b))

deriving instance (Show (PData Global a), Show (PData Global b))
        => Show (PData Global (a, b))


instance (PS a, PS b) => PS (a, b) where
  emptyPS			
   = PTuple2 emptyPS emptyPS

  appPS (PTuple2 v11 v12) (PTuple2 v21 v22)
   = PTuple2 (v11 `appPS` v21) (v12 `appPS` v22)
	
  fromListPS xx
   = let (xs, ys)	= unzip xx
     in	 PTuple2 (fromListPS xs) (fromListPS ys)

instance (PE a, PE b) => PE (a, b)

instance (PJ m a, PJ m b) => PJ m (a, b) where
  restrictPJ n (PTuple2 x y)
        = PTuple2 (restrictPJ n x) (restrictPJ n y)


instance (PR a, PR b) => PR (a, b)


unzipPA :: PArray (a, b) -> (PArray a, PArray b)
unzipPA (PArray n (PTuple2 xs ys))
        = (PArray n xs, PArray n ys)

unzipPA_l :: forall m1 a b. PJ m1 (PArray (a, b))
          => Int -> PData m1 (PArray (a, b)) -> PData Sized (PArray a, PArray b)
unzipPA_l c vs
 = case restrictPJ c vs of
         PNestedS segd (PTuple2 xs ys)
          -> PTuple2 (PNestedS segd xs) (PNestedS segd ys)