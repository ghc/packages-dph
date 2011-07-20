{-# LANGUAGE
        TypeOperators,
        FlexibleInstances,
        MultiParamTypeClasses #-}

module Data.Array.Parallel.PArray.PData.Closure where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Closure


instance PS (a :-> b) where
  {-# INLINE_PDATA emptyPS #-}
  emptyPS 
	= AClo 	(\_ _ -> error "empty array closure")
 		(\_ _ -> error "empty array closure")
		(emptyPS :: PData Sized ())

  {-# INLINE_PDATA appPS #-}
  appPS		= error "appPR[:->] not defined"

  {-# INLINE_PDATA fromListPS #-}
  fromListPS	= error "fromListPR[:->] not defined"


instance PJ m (a :-> b) where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n (AClo fv fl env)
        = AClo fv fl (restrictPJ n env)

  {-# INLINE_PDATA restrictsPJ #-}
  restrictsPJ segd (AClo fv fl env) 
        = AClo fv fl (restrictsPJ segd env)

  {-# INLINE_PDATA indexPJ #-}
  indexPJ   (AClo fv fl env) ix 
	= Clo fv fl (indexPJ env ix)
        

instance PE (a :-> b) where
  {-# INLINE_PDATA repeatPE #-}
  repeatPE (Clo fv fl env)
	= AClo fv fl (repeatPE env)


instance PR (a :-> b)
