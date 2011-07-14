{-# LANGUAGE
        TypeOperators,
        FlexibleInstances,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PData.Closure where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Closure


instance PS (a :-> b) where
  emptyPS 
	= AClo 	(\_ _ -> error "empty array closure")
 		(\_ _ -> error "empty array closure")
		(emptyPS :: PData Sized ())

  appPS		= error "appPR[:->] not defined"
  fromListPS	= error "fromListPR[:->] not defined"



instance PJ m (a :-> b) where
  restrictPJ n (AClo fv fl env)
        = AClo fv fl (restrictPJ n env)

  indexPJ   (AClo fv fl env) ix 
	= Clo fv fl (indexPJ env ix)
        
  replicatelPJ segd (AClo fv fl env) 
        = AClo fv fl (replicatelPJ segd env)

instance PE (a :-> b) where
  repeatPE (Clo fv fl env)
	= AClo fv fl (repeatPE env)


instance PR (a :-> b)
