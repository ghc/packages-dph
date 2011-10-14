
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

-- Tuple2 ---------------------------------------------------------------------
type instance PRepr (a,b)
        = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a,b) where
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  toArrPRepr   (PTuple2 as bs)
        = PTuple2 (PWrap as) (PWrap bs)

  fromArrPRepr (PTuple2 (PWrap as) (PWrap bs))
        = PTuple2 as bs


-- PD Wrappers ----------------------------------------------------------------
-- These wrappers have the same types in the ones in D.A.P.PArray.PData.Tuple,
-- except that they take a PA dictionary instead of a PR dictionary.
--
-- See D.A.P.PArray.PRepr.Base   for docs on why we need the wrappers.
-- See D.A.P.PArray.PData.Tuple  for docs on what the PR versions do.
--
{-# INLINE unziplPD #-}
unziplPD        :: (PA a, PA b) => PData (PArray (a, b)) -> PData (PArray a, PArray b)
unziplPD arr    = error "unziplPD: not implemented"

