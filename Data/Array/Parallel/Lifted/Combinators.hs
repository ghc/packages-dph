module Data.Array.Parallel.Lifted.Combinators (
  mapPA
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure

mapPA_v :: PA a -> PA b -> (a :-> b) -> PArray a -> PArray b
{-# INLINE mapPA_v #-}
mapPA_v pa pb f as = replicatePA (dPA_Clo pa pb) (lengthPA pa as) f
                     $:^ as

mapPA_l :: PA a -> PA b
        -> PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
{-# INLINE mapPA_l #-}
mapPA_l pa pb fs as = error "mapPA_l"

mapPA :: PA a -> PA b -> ((a :-> b) :-> PArray a :-> PArray b)
{-# INLINE mapPA #-}
mapPA pa pb = closure2 (dPA_Clo pa pb) (mapPA_v pa pb) (mapPA_l pa pb)

