{-# LANGUAGE
        CPP,
        TypeOperators,
        FlexibleInstances,
        MultiParamTypeClasses #-}
#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Closure where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Closure


instance PR (a :-> b) where
  {-# INLINE_PDATA validPR #-}
  validPR (AClo _ _ env)
        = validPR env

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = AClo  (\_ _ -> error "empty array closure")
                (\_ _ -> error "empty array closure")
                (emptyPR :: PData ())

  {-# INLINE_PDATA nfPR #-}
  nfPR (AClo fv fl envs)
        = fv `seq` fl `seq` nfPR envs `seq` ()

  {-# INLINE_PDATA lengthPR #-}
  lengthPR (AClo _ _ envs)
        = lengthPR envs

  {-# INLINE_PDATA replicatePR #-}
  replicatePR n (Clo fv fl envs)
        = AClo fv fl (replicatePR n envs)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (AClo fv fl envs)
        = AClo fv fl (replicatesPR lens envs)

  {-# INLINE_PDATA indexPR #-}
  indexPR (AClo fv fl envs) ix
        = Clo fv fl (indexPR envs ix)

  {-# INLINE_PDATA extractPR #-}
  extractPR (AClo fv fl envs) start len
        = AClo fv fl (extractPR envs start len)
        

  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (AClo fv fl envs) tags tag
        = AClo fv fl (packByTagPR envs tags tag)


  -- TODO: not sure about these.
  --       we can't just extract the env because the vector may
  --       contain closures with multiple functions.
  {-# INLINE_PDATA extractsPR #-}
  indexlPR      = error     "indexlPR[:->]: not defined"
  extractsPR    = error    "extractPR[:->]: not defined"
  appendPR      = error     "appendPR[:->]: not defined"
  appendsPR     = error     "appendPR[:->]: not defined"
  combine2PR    = error    "combinePR[:->]: not defined"
  fromVectorPR  = error "fromVectorPR[:->]: not defined"
  toVectorPR    = error   "toVectorPR[:->]: not defined"
  fromUArrayPR  = error "fromUArrayPR[:->]: not defined"
  toUArrayPR    = error   "toUArrayPR[:->]: not defined"