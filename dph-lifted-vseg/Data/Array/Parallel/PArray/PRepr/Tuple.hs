#include "fusion-phases.h"

-- | PRepr instance for tuples
--   and PD wrappers for other functions defined in D.A.P.PArray.PData.Tuple.
module Data.Array.Parallel.PArray.PRepr.Tuple
        (unziplPD)
where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Wrap
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Nested


-- Tuple2 ---------------------------------------------------------------------
type instance PRepr (a,b)
        = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a,b) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr   (PTuple2 as bs)
        = PTuple2 (PWrap as) (PWrap bs)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PTuple2 (PWrap as) (PWrap bs))
        = PTuple2 as bs

  -- PROBLEM: What to do here?
  {-# INLINE_PA toNestedArrPRepr #-}
  toNestedArrPRepr (PNested _vsegd _pdatas)
        = error "Data.Array.Parallel.PArray.PRepr.Tuple: doh, what do do?"
        


-- PD Wrappers ----------------------------------------------------------------
-- These wrappers have the same types in the ones in D.A.P.PArray.PData.Tuple,
-- except that they take a PA dictionary instead of a PR dictionary.
--
-- See D.A.P.PArray.PRepr.Base   for docs on why we need the wrappers.
-- See D.A.P.PArray.PData.Tuple  for docs on what the PR versions do.
--
{-# INLINE_PA unziplPD #-}
unziplPD        :: (PA a, PA b) => PData (PArray (a, b)) -> PData (PArray a, PArray b)
unziplPD arr    = error "unziplPD: not implemented"

