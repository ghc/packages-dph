#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Wrap where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Vector.Unboxed                      (Vector)
import qualified Data.Vector                    as V

-------------------------------------------------------------------------------
newtype instance PData (Wrap a)
        = PWrap (PData a)

newtype instance PDatas (Wrap a)
        = PWraps (PDatas a)


-- PR -------------------------------------------------------------------------
instance PA a => PR (Wrap a) where       

  {-# INLINE_PDATA validPR #-}
  validPR (PWrap pdata)  
        = validPA pdata
 
  {-# INLINE_PDATA emptyPR #-}
  emptyPR               
        = PWrap emptyPA
  
  {-# INLINE_PDATA nfPR #-}
  nfPR (PWrap pdata)      
        = nfPA pdata
        
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PWrap pdata)
        = lengthPA pdata
        
  {-# INLINE_PDATA replicatePR #-}
  replicatePR n (Wrap x)
        = PWrap $ replicatePA n x

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PWrap xs)
        = PWrap $ replicatesPA segd xs

  {-# INLINE_PDATA indexPR #-}
  indexPR (PWrap xs) ix
        = Wrap  $ indexPA xs ix

  {-# INLINE_PDATA indexlPR #-}
  indexlPR (PNested vsegd (PWraps pdatas)) ixs
        = PWrap (indexlPA (PNested vsegd pdatas) ixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PWrap xs) ix n
        = PWrap $ extractPA xs ix n
        
  {-# INLINE_PDATA extractsPR #-}
  extractsPR (PWraps pdatas) ssegd
        = PWrap $ extractsPA pdatas ssegd

  {-# INLINE_PDATA appendPR #-}
  appendPR (PWrap xs) (PWrap ys)
        = PWrap $ appendPA xs ys
        
  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PWrap xs) segd2 (PWrap ys)
        = PWrap $ appendsPA segdResult segd1 xs segd2 ys
        
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PWrap xs) tags tag
        = PWrap $ packByTagPA xs tags tag

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PWrap xs) (PWrap ys)
        = PWrap $ combine2PA sel xs ys

  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR vec 
        = PWrap $ fromVectorPA $ V.map unWrap vec
        
  {-# INLINE_PDATA toVectorPR #-}
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
        
{-
  {-# INLINE_PDATA concatdPR #-}
  concatdPR vecs
        = PWraps
                $ V.concat $ V.toList
                $ V.map (\(PWraps xs) -> toVectordPA xs) vecs
-}
{-                
  {-# INLINE_PDATA mapdPR #-}
  mapdPR f (PDoubles uarrs)
        = PDoubles 
                $ V.map (\xs -> case f (PDouble xs) of 
                                        PDouble zs' -> zs')
                $ uarrs

  {-# INLINE_PDATA zipWithdPR #-}
  zipWithdPR f (PDoubles uarrs1) (PDoubles uarrs2)
        = PDoubles
                $ V.zipWith 
                        (\xs ys -> case f (PDouble xs) (PDouble ys) of
                                        PDouble zs' -> zs')
                        uarrs1 uarrs2
                                
  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = PDoubles $ V.map (\(PDouble xs) -> xs) vec
        
  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PDoubles vec)
        = V.map PDouble vec
-}