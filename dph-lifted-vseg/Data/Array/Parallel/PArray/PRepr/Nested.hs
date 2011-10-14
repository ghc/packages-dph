
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
  {-# INLINE toPRepr #-}
  toPRepr (PArray n xs) 
        = PArray n (toArrPRepr xs)

  {-# INLINE fromPRepr #-}
  fromPRepr (PArray n xs)
        = PArray n (fromArrPRepr xs)

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PNested segd xs)
        = PNested segd (V.map toArrPRepr xs)    -- TODO: this isn't O(1)

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PNested segd xs)
        = PNested segd (V.map fromArrPRepr xs)  -- TODO: this isn't O(1)


-- PD Wrappers ----------------------------------------------------------------
-- These wrappers have the same types in the ones in D.A.P.PArray.PData.Nested,
-- except that they take a PA dictionary instead of a PR dictionary.
--
-- See D.A.P.PArray.PRepr.Base   for docs on why we need the wrappers.
-- See D.A.P.PArray.PData.Nested for docs on what the PR versions do.
--
{-# INLINE concatPD #-}
concatPD        :: PA a => PData (PArray a) -> PData a
concatPD arr
 = fromArrPRepr $ concatPR $ toArrPRepr arr
 
 
{-# INLINE unconcatPD #-}
unconcatPD      :: (PA a, PA b) => PData (PArray a) -> PData b -> PData (PArray b)
unconcatPD arr1 arr2
 = fromArrPRepr $ unconcatPR (toArrPRepr arr1) (toArrPRepr arr2)


{-# INLINE concatlPD #-}
concatlPD       :: PA a => PData (PArray (PArray a)) -> PData (PArray a)
concatlPD arr
 = fromArrPRepr $ concatlPR (toArrPRepr arr)


{-# INLINE appendlPD #-}
appendlPD       :: PA a => PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPD arr1 arr2
 = fromArrPRepr $ appendlPR (toArrPRepr arr1) (toArrPRepr arr2)
