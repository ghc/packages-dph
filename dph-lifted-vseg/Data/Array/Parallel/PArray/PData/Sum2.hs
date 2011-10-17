#include "fusion-phases.h"
module Data.Array.Parallel.PArray.PData.Sum2 where

import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.Unlifted             as U


-------------------------------------------------------------------------------
data instance PData (Sum2 a b)
        = PSum2 U.Sel2 (PData a) (PData b)
        
        
-- PR -------------------------------------------------------------------------

-- This stuff isn't implemented yet.
nope    = error "Data.Array.Parallel.PData.Void: no PR method for Sum2"


instance (PR a, PR b) => PR (Sum2 a b)  where
  {-# INLINE_PDATA validPR #-}
  validPR _
        = True

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = nope

  {-# INLINE_PDATA nfPR #-}
  nfPR (PSum2 sel xs ys)
        = nope

  {-# INLINE_PDATA lengthPR #-}
  lengthPR _
        = nope

  {-# INLINE_PDATA replicatePR #-}
  replicatePR _
        = nope
                
  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR
        = nope
                      
  {-# INLINE_PDATA indexPR #-}
  indexPR
        = nope
                
  {-# INLINE_PDATA indexlPR #-}
  indexlPR 
        = nope
  
  {-# INLINE_PDATA extractPR #-}
  extractPR 
        = nope
  
  {-# INLINE_PDATA extractsPR #-}
  extractsPR 
        = nope
        
  {-# INLINE_PDATA appendPR #-}
  appendPR
        = nope
        
  {-# INLINE_PDATA appendsPR #-}
  appendsPR
        = nope
        
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR
        = nope
  
  {-# INLINE_PDATA combine2PR #-}
  combine2PR 
        = nope
        
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR 
        = nope
        
  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR
        = nope
  