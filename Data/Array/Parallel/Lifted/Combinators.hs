module Data.Array.Parallel.Lifted.Combinators (
  closure1, closure2, closure3,
  mapPA, zipWithPA
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances

closure1 :: (a -> b) -> (PArray a -> PArray b) -> (a :-> b)
{-# INLINE closure1 #-}
closure1 fv fl = Clo dPA_Unit (\_ -> fv) (\_ -> fl) ()

closure2 :: PA a
         -> (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)
{-# INLINE closure2 #-}
closure2 pa fv fl = Clo dPA_Unit fv_1 fl_1 ()
  where
    fv_1 _ x  = Clo  pa fv fl x
    fl_1 _ xs = AClo pa fv fl xs

closure3 :: PA a -> PA b
         -> (a -> b -> c -> d)
         -> (PArray a -> PArray b -> PArray c -> PArray d)
         -> (a :-> b :-> c :-> d)
{-# INLINE closure3 #-}
closure3 pa pb fv fl = Clo dPA_Unit fv_1 fl_1 ()
  where
    fv_1 _  x  = Clo  pa fv_2 fl_2 x
    fl_1 _  xs = AClo pa fv_2 fl_2 xs

    fv_2 x  y  = Clo  (dPA_2 pa pb) fv_3 fl_3 (x,y)
    fl_2 xs ys = AClo (dPA_2 pa pb) fv_3 fl_3 (P_2 (lengthPA# pa xs) xs ys)

    fv_3 (x,y) z = fv x y z
    fl_3 (P_2 _ xs ys) zs = fl xs ys zs

mapPA_v :: PA a -> PA b -> (a :-> b) -> PArray a -> PArray b
{-# INLINE mapPA_v #-}
mapPA_v pa pb f as = replicatePA# (dPA_Clo pa pb) (lengthPA# pa as) f
                     $:^ as

mapPA_l :: PA a -> PA b
        -> PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
{-# INLINE mapPA_l #-}
mapPA_l pa pb fs as = error "mapPA_l"

mapPA :: PA a -> PA b -> ((a :-> b) :-> PArray a :-> PArray b)
{-# INLINE mapPA #-}
mapPA pa pb = closure2 (dPA_Clo pa pb) (mapPA_v pa pb) (mapPA_l pa pb)

zipWithPA_v :: PA a -> PA b -> PA c
            -> (a :-> b :-> c) -> PArray a -> PArray b -> PArray c
{-# INLINE zipWithPA_v #-}
zipWithPA_v pa pb pc f as bs = replicatePA# (dPA_Clo pa (dPA_Clo pb pc))
                                            (lengthPA# pa as)
                                            f
                                $:^ as $:^ bs

zipWithPA_l :: PA a -> PA b -> PA c
            -> PArray (a :-> b :-> c) -> PArray (PArray a) -> PArray (PArray b)
            -> PArray (PArray c)
{-# INLINE zipWithPA_l #-}
zipWithPA_l pa pb pc f as bs = error "zipWithPA_l"

zipWithPA :: PA a -> PA b -> PA c
          -> ((a :-> b :-> c) :-> PArray a :-> PArray b :-> PArray c)
{-# INLINE zipWithPA #-}
zipWithPA pa pb pc = closure3 (dPA_Clo pa (dPA_Clo pb pc)) (dPA_PArray pa)
                              (zipWithPA_v pa pb pc)
                              (zipWithPA_l pa pb pc)

