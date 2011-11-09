#include "fusion-phases.h"

-- | PRepr instance for tuples
--   and PD wrappers for other functions defined in D.A.P.PArray.PData.Tuple.
module Data.Array.Parallel.PArray.PRepr.Tuple
        ( PRepr
        , zipl)
where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Wrap


-------------------------------------------------------------------------------
type instance PRepr (a,b)
        = (Wrap a, Wrap b)


-- PA -------------------------------------------------------------------------
instance (PA a, PA b) => PA (a,b) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PTuple2 as bs)
        = PTuple2 (PWrap as) (PWrap bs)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple2 (PWrap as) (PWrap bs))
        = PTuple2 as bs

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PTuple2s as bs)
        = PTuple2s (PWraps as) (PWraps bs)

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PTuple2s (PWraps as) (PWraps bs))
        = PTuple2s as bs


-- | Lifted zip.
zipl    :: (PA a, PA b)
        => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
zipl (PArray n# xs) (PArray _ ys)
        = PArray n# $ ziplPA xs ys


ziplPA  :: (PA a, PA b) 
        => PData (PArray a) -> PData (PArray b) -> PData (PArray (a, b))
ziplPA xs ys
 = let  PNested vsegd (PTuple2s xs' ys')
         = ziplPR (toNestedArrPRepr xs)
                  (toNestedArrPRepr ys)

   in   PNested vsegd (PTuple2s   
                        (fromArrPReprs xs')
                        (fromArrPReprs ys'))


