{-# OPTIONS_HADDOCK hide #-}
#include "fusion-phases.h"

-- | PR instance for the Wrap type.
module Data.Array.Parallel.PArray.PData.Wrap where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import qualified Data.Vector                    as V

-------------------------------------------------------------------------------
newtype instance PData (Wrap a)
        = PWrap (PData a)

newtype instance PDatas (Wrap a)
        = PWraps (PDatas a)


-- PR -------------------------------------------------------------------------
instance PA a => PR (Wrap a) where       

  {-# NOINLINE validPR #-}
  validPR (PWrap pdata)  
        = validPA pdata

  {-# NOINLINE nfPR #-}
  nfPR (PWrap pdata)      
        = nfPA pdata

  {-# NOINLINE similarPR #-}
  similarPR (Wrap x) (Wrap y)
        = similarPA x y

  {-# NOINLINE coversPR #-}
  coversPR weak (PWrap pdata) ix
        = coversPA weak pdata ix

  {-# NOINLINE pprpPR #-}
  pprpPR (Wrap x)
        = pprpPA x

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PWrap pdata)
        = pprpDataPA pdata


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR               
        = PWrap emptyPA
  
  {-# INLINE_PDATA replicatePR #-}
  replicatePR n (Wrap x)
        = PWrap $ replicatePA n x

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PWrap xs)
        = PWrap $ replicatesPA segd xs

  {-# INLINE_PDATA appendPR #-}
  appendPR (PWrap xs) (PWrap ys)
        = PWrap $ appendPA xs ys
        
  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PWrap xs) segd2 (PWrap ys)
        = PWrap $ appendsPA segdResult segd1 xs segd2 ys
        

  -- Projections --------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PWrap xs)
        = lengthPA xs
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PWrap xs) ix
        = Wrap  $ indexPA xs ix

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PWraps pdatas) srcs ixs
        = PWrap $ indexsPA pdatas srcs ixs

  {-# NOINLINE extractPR #-}
  extractPR (PWrap xs) ix n
        = PWrap $ extractPA xs ix n
        
  {-# NOINLINE extractsPR #-}
  extractsPR (PWraps pdatas) ssegd
        = PWrap $ extractsPA pdatas ssegd


  -- Pack and Combine ---------------------------
  {-# NOINLINE packByTagPR #-}
  packByTagPR (PWrap xs) tags tag
        = PWrap $ packByTagPA xs tags tag

  {-# NOINLINE combine2PR #-}
  combine2PR sel (PWrap xs) (PWrap ys)
        = PWrap $ combine2PA sel xs ys


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR vec 
        = PWrap $ fromVectorPA $ V.map unWrap vec
        
  {-# NOINLINE toVectorPR #-}
  toVectorPR (PWrap pdata)
        = V.map Wrap $ toVectorPA pdata


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PWraps emptydPA

  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PWrap pdata)
        = PWraps $ singletondPA pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PWraps pdatas)
        = lengthdPA pdatas
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PWraps pdatas) ix
        = PWrap $ indexdPA pdatas ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PWraps xs) (PWraps ys)
        = PWraps $ appenddPA xs ys

  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
        = PWraps $ fromVectordPA $ V.map (\(PWrap x) -> x) vec

  {-# NOINLINE toVectordPR #-}
  toVectordPR (PWraps pdatas)
        = V.map PWrap $ toVectordPA pdatas
        


