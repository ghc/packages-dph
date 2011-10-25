#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Unit where
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Text.PrettyPrint


-------------------------------------------------------------------------------
-- | NOTE: We're only maintaining the length temporarilly. 
--         This is done so that the validPA checks work, but in future we'll 
--         fix validPA so it handles 'defined everywhere' arrays.
data instance PData ()
        = PUnit Int

data instance PDatas ()
        = PUnits (V.Vector (PData ()))

punit :: PData ()
punit =  PUnit 0


-- PR -------------------------------------------------------------------------
instance PR () where
  {-# INLINE_PDATA validPR #-}
  validPR _
        = True

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PUnit 0

  {-# INLINE_PDATA nfPR #-}
  nfPR xx
        = xx `seq` ()

  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PUnit n)
        = n

  {-# INLINE_PDATA replicatePR #-}
  replicatePR n _
        = PUnit n

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd _
        = PUnit (U.elementsSegd segd)
        
  {-# INLINE_PDATA indexPR #-}
  indexPR _ _
        = ()

  {-# INLINE_PDATA indexlPR #-}
  indexlPR _ (PInt uarr)
        = PUnit $ U.length uarr

  {-# INLINE_PDATA extractPR #-}
  extractPR _ _ len
        = PUnit len
        
  {-# INLINE_PDATA extractsPR #-}
  extractsPR _ ussegd
        = PUnit (U.sum $ U.lengthsSSegd ussegd)
                
  {-# INLINE_PDATA appendPR #-}
  appendPR (PUnit len1) (PUnit len2)
        = PUnit (len1 + len2)

  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult _ _ _ _
        = PUnit (U.lengthSegd segdResult)
        
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR _ tags tag
        = PUnit (U.length $ U.filter (== tag) tags)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel2 _ _
        = PUnit ( U.elementsSel2_0 sel2
                + U.elementsSel2_1 sel2)

  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR vec
        = PUnit (V.length vec)

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR (PUnit len)
        = V.replicate len ()

  -----------------------------------------------
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PUnits pdatas)
        = V.length pdatas
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PUnits pdatas) ix
        = pdatas `V.unsafeIndex` ix
        


-- Show -----------------------------------------------------------------------
deriving instance Show (PData ())

instance PprPhysical (PData ()) where
  pprp uu
   = text $ show uu

instance PprVirtual (PData ()) where
  pprv (PUnit n)
   = text $ "[ () x " ++ show n ++ " ]"

