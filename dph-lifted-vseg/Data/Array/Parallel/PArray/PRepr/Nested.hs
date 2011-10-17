#include "fusion-phases.h"

-- | PRepr instance for nested arrays, 
--   and PD wrappers for other functions defined in D.A.P.PArray.PData.Nested.
module Data.Array.Parallel.PArray.PRepr.Nested
        ( concatPD
        , unconcatPD
        , concatlPD
        , appendlPD)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted           as U
import qualified Data.Vector                            as V
import Data.Vector                                      (Vector)


-- PArray ---------------------------------------------------------------------
type instance PRepr (PArray a)
        = PArray (PRepr a)

instance PA a => PA (PArray a) where
  {-# INLINE_PA toPRepr #-}
  toPRepr (PArray n xs) 
        = PArray n $ toArrPRepr xs

  {-# INLINE_PA fromPRepr #-}
  fromPRepr (PArray n xs)
        = PArray n $ fromArrPRepr xs

  {-# INLINE_PA toArrPRepr #-}
  toArrPRepr (PNested segd xs)
        = PNested segd $ toArrPReprs xs

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PNested segd xs)
        = PNested segd $ fromArrPReprs xs

  {-# INLINE_PA toNestedArrPRepr #-}
  toNestedArrPRepr (PNested segd pdatas)
        = PNested segd $ toArrPReprs pdatas


-- PD Wrappers ----------------------------------------------------------------
-- These wrappers have the same types in the ones in D.A.P.PArray.PData.Nested,
-- except that they take a PA dictionary instead of a PR dictionary.
--
-- See D.A.P.PArray.PRepr.Base   for docs on why we need the wrappers.
-- See D.A.P.PArray.PData.Nested for docs on what the PR versions do.
--
{-# INLINE_PA concatPD #-}
concatPD        :: PA a => PData (PArray a) -> PData a
concatPD arr
 = fromArrPRepr $ concatPR $ toArrPRepr arr
 
 
{-# INLINE_PA unconcatPD #-}
unconcatPD      :: (PA a, PA b) => PData (PArray a) -> PData b -> PData (PArray b)
unconcatPD arr1 arr2
 = fromArrPRepr $ unconcatPR (toArrPRepr arr1) (toArrPRepr arr2)


{-# INLINE_PA concatlPD #-}
concatlPD       :: PA a => PData (PArray (PArray a)) -> PData (PArray a)
concatlPD arr
 = fromArrPRepr $ concatlPR (toArrPRepr arr)


{-# INLINE_PA appendlPD #-}
appendlPD       :: PA a => PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPD arr1 arr2
 = fromArrPRepr $ appendlPR (toArrPRepr arr1) (toArrPRepr arr2)
