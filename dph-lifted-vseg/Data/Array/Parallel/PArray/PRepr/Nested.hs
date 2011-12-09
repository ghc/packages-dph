{-# OPTIONS_HADDOCK hide #-}
#include "fusion-phases.h"

-- | PRepr/PA instance for nested arrays, 
--   and PA wrappers for other functions defined in D.A.P.PArray.PData.Nested.
module Data.Array.Parallel.PArray.PRepr.Nested
        ( mkPNestedPA
        , concatPA,  concatlPA
        , unconcatPA
        , appendlPA
        , indexlPA
        , slicelPA)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V


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
  toArrPRepr (PNested segd xs flat)
        = PNested segd (toArrPReprs xs) (toArrPRepr flat)

  {-# INLINE_PA fromArrPRepr #-}
  fromArrPRepr (PNested segd xs flat)
        = PNested segd (fromArrPReprs xs) (fromArrPRepr flat)

  {-# INLINE_PA toArrPReprs #-}
  toArrPReprs (PNesteds vec)
        = PNesteds $ V.map toArrPRepr vec

  {-# INLINE_PA fromArrPReprs #-}
  fromArrPReprs (PNesteds vec)
        = PNesteds $ V.map fromArrPRepr vec


-- PA Wrappers ----------------------------------------------------------------
-- These wrappers have the same types in the ones in D.A.P.PArray.PData.Nested,
-- except that they take a PA dictionary instead of a PR dictionary.
--
-- See D.A.P.PArray.PRepr.Base   for docs on why we need the wrappers.
-- See D.A.P.PArray.PData.Nested for docs on what the PR versions do.
--
-- | Conatruct a nested array.
mkPNestedPA 
        :: PA a
        => U.Array Int        -- ^ Virtual segment ids.
        -> U.Array Int        -- ^ Lengths of physical segments.
        -> U.Array Int        -- ^ Starting indices of physical segments.
        -> U.Array Int        -- ^ Source id (what chunk to get each segment from).
        -> PDatas a           -- ^ Chunks of array data.
        -> PData (PArray a)

mkPNestedPA vsegids pseglens psegstart psegsrcs pdatas
 = let  pdatas' = toArrPReprs pdatas
   in   fromArrPRepr $ mkPNested vsegids pseglens psegstart psegsrcs pdatas'


{-# INLINE_PA concatPA #-}
concatPA        :: PA a => PData (PArray a) -> PData a
concatPA arr
 = fromArrPRepr $ concatPR $ toArrPRepr arr
 
 
{-# INLINE_PA unconcatPA #-}
unconcatPA      :: (PA a, PA b) => PData (PArray a) -> PData b -> PData (PArray b)
unconcatPA arr1 arr2
 = fromArrPRepr $ unconcatPR (toArrPRepr arr1) (toArrPRepr arr2)


{-# INLINE_PA concatlPA #-}
concatlPA       :: PA a => PData (PArray (PArray a)) -> PData (PArray a)
concatlPA arr
 = fromArrPRepr $ concatlPR (toArrPRepr arr)


{-# INLINE_PA appendlPA #-}
appendlPA       :: PA a => PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPA arr1 arr2
 = fromArrPRepr $ appendlPR (toArrPRepr arr1) (toArrPRepr arr2)


{-# INLINE_PA indexlPA #-}
indexlPA        :: PA a => PData (PArray a) -> PData Int -> PData a
indexlPA arr ixs
 = fromArrPRepr $ indexlPR (toArrPRepr arr) ixs

{-# INLINE_PA slicelPA #-}
slicelPA        :: PA a => PData Int -> PData Int -> PData (PArray a) -> PData (PArray a)
slicelPA starts lens arr
 = fromArrPRepr $ slicelPR starts lens (toArrPRepr arr)

