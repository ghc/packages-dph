{-# LANGUAGE
        TypeOperators,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PDataClosure where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.Lifted.Closure


instance PS (a :-> b) where
  emptyPS 
	= AClo 	(\_ _ -> error "empty array closure")
 		(\_ _ -> error "empty array closure")
		(emptyPS :: PData Sized ())

  appPS		= error "appPR[:->] not defined"
  fromListPS	= error "fromListPR[:->] not defined"


instance PJ Global (a :-> b) where
  restrictPJ n (AClo fv fl env)	
	= AClo fv fl (restrictPJ n env)

  indexPJ   (AClo fv fl env) ix
	= Clo fv fl (indexPJ env ix)


instance PJ Sized (a :-> b) where
  restrictPJ n (AClo fv fl env)	
	= AClo fv fl (restrictPJ n env)

  indexPJ   (AClo fv fl env) ix 
	= Clo fv fl (indexPJ env ix)


instance PE (a :-> b) where
  repeatPE (Clo fv fl env)
	= AClo fv fl (repeatPE env)


instance PR (a :-> b)
