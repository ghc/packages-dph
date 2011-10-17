#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Void 
        (Void, pvoid)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base


-------------------------------------------------------------------------------
-- | The Void type is used when representing enumerations. 
--   A type like Bool is represented as @Sum2 Void Void@, meaning that we only
--   only care about the tag of the data constructor and not its argumnent.
--
data instance PData Void

pvoid :: PData Void
pvoid   = error "Data.Array.Parallel.PData.Void"


-- PR --------------------------------------------------------------------------
nope    = error "Data.Array.Parallel.PData.Void: no PR method for void"

instance PR Void where
  {-# INLINE_PDATA validPR #-}
  validPR       = nope

  {-# INLINE_PDATA emptyPR #-}
  emptyPR       = nope

  {-# INLINE_PDATA nfPR #-}
  nfPR          = nope

  {-# INLINE_PDATA lengthPR #-}
  lengthPR      = nope

  {-# INLINE_PDATA replicatePR #-}
  replicatePR   = nope

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR  = nope
                
  {-# INLINE_PDATA indexPR #-}
  indexPR       = nope

  {-# INLINE_PDATA indexlPR #-}
  indexlPR      = nope

  {-# INLINE_PDATA extractPR #-}
  extractPR     = nope

  {-# INLINE_PDATA extractsPR #-}
  extractsPR    = nope

  {-# INLINE_PDATA appendPR #-}
  appendPR      = nope
  
  {-# INLINE_PDATA appendsPR #-}
  appendsPR     = nope

  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR   = nope

  {-# INLINE_PDATA combine2PR #-}
  combine2PR    = nope

  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR  = nope

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR    = nope