{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        StandaloneDeriving, ExplicitForAll,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PData.Tuple 
where
import Data.Array.Parallel.PArray.PData.Base
-- import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted as U


data instance PData (a, b)
        = PTuple2 (PData a) (PData b)

deriving instance (Show (PData a), Show (PData b)) 
	=> Show (PData (a, b))


instance (PR a, PR b) => PR (a, b) where
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
   = PTuple2 emptyPR emptyPR

  {-# INLINE_PDATA nfPR #-}
  nfPR (PTuple2 arr1 arr2)
        = nfPR arr2 `seq` nfPR arr2 `seq` ()

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y)
        = PTuple2 (replicatePR len x) (replicatePR len y)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple2 arr1 arr2)
        = PTuple2 (replicatesPR lens arr1) (replicatesPR lens arr2)

  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple2 arr1 arr2) ix
        = (indexPR arr1 ix, indexPR arr2 ix)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple2 arr1 arr2) start len
        = PTuple2 (extractPR arr1 start len) 
                  (extractPR arr2 start len)

  {-# INLINE_PDATA appPR #-}
  appPR (PTuple2 arr11 arr12) (PTuple2 arr21 arr22)
   = PTuple2 (arr11 `appPR` arr21) (arr12 `appPR` arr22)

  {-# INLINE_PDATA fromListPR #-}
  fromListPR xx
   = let (xs, ys)	= unzip xx
     in	 PTuple2 (fromListPR xs) (fromListPR ys)

  {-# INLINE_PDATA fromUArrayPR #-}
  fromUArrayPR xx
   = error "fromUArrayPR[Tuple]: put in Scalar class, need U.Elt dictionary"
   
  {-# INLINE_PDATA toUArrayPR #-}
  toUArrayPR xx
   = error "toUArrayPR[Tuple]: put in Scalar class, need U.Elt dictionary"
   

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

{-
{-# INLINE_PA unzipPA_l #-}
unzipPA_l :: forall m1 a b. PJ m1 (PArray (a, b))
          => Int -> PData m1 (PArray (a, b)) -> PData Sized (PArray a, PArray b)
unzipPA_l c vs
 = case restrictPJ c vs of
         PNestedS segd (PTuple2 xs ys)
          -> PTuple2 (PNestedS segd xs) (PNestedS segd ys)
-}


