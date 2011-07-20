{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        StandaloneDeriving, ExplicitForAll,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PData.Tuple 
where
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
  {-# INLINE_PDATA emptyPS #-}
  emptyPS			
   = PTuple2 emptyPS emptyPS

  {-# INLINE_PDATA appPS #-}
  appPS (PTuple2 v11 v12) (PTuple2 v21 v22)
   = PTuple2 (v11 `appPS` v21) (v12 `appPS` v22)
	
  {-# INLINE_PDATA fromListPS #-}
  fromListPS xx
   = let (xs, ys)	= unzip xx
     in	 PTuple2 (fromListPS xs) (fromListPS ys)


instance (PE a, PE b) => PE (a, b)


instance (PJ m a, PJ m b) => PJ m (a, b) where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n (PTuple2 x y)
        = PTuple2 (restrictPJ n x) (restrictPJ n y)


instance (PR a, PR b) => PR (a, b)


-- | Zip a pair of arrays into an array of pairs.
--   The two arrays must have the same length, else `error`. 
{-# INLINE_PA zipPA #-}
zipPA :: PArray a -> PArray b -> PArray (a, b)
zipPA (PArray n1 xs) (PArray n2 ys)
        | n1 == n2
        = PArray n1 (PTuple2 xs ys)
        
        | otherwise
        = error "Data.Array.Parallel.PArray.zipPA: arrays are not the same length"


-- | Unzip an array of pairs into a pair of arrays.
{-# INLINE_PA unzipPA #-}
unzipPA :: PArray (a, b) -> (PArray a, PArray b)
unzipPA (PArray n (PTuple2 xs ys))
        = (PArray n xs, PArray n ys)

{-# INLINE_PA unzipPA_l #-}
unzipPA_l :: forall m1 a b. PJ m1 (PArray (a, b))
          => Int -> PData m1 (PArray (a, b)) -> PData Sized (PArray a, PArray b)
unzipPA_l c vs
 = case restrictPJ c vs of
         PNestedS segd (PTuple2 xs ys)
          -> PTuple2 (PNestedS segd xs) (PNestedS segd ys)



