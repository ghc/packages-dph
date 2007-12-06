module Data.Array.Parallel.Lifted.Combinators (
  closure1, closure2, closure3,
  replicatePA, singletonPA, mapPA, crossMapPA, zipWithPA,
  packPA, filterPA, indexPA
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances

import GHC.Exts (Int(..))

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

replicatePA_v :: PA a -> Int -> a -> PArray a
{-# INLINE replicatePA_v #-}
replicatePA_v pa (I# n#) x = replicatePA# pa n# x

replicatePA_l :: PA a -> PArray Int -> PArray a -> PArray (PArray a)
{-# INLINE replicatePA_l #-}
replicatePA_l pa (PInt n# ns) xs
  = PNested n# ns (unsafe_scanPA_Int# (+) 0 ns)
                  (replicatelPA# pa (sumPA_Int# ns) ns xs)

replicatePA :: PA a -> (Int :-> a :-> PArray a)
{-# INLINE replicatePA #-}
replicatePA pa = closure2 dPA_Int (replicatePA_v pa) (replicatePA_l pa)

singletonPA_v :: PA a -> a -> PArray a
{-# INLINE singletonPA_v #-}
singletonPA_v pa x = replicatePA_v pa 1 x

singletonPA_l :: PA a -> PArray a -> PArray (PArray a)
{-# INLINE singletonPA_l #-}
singletonPA_l pa xs
  = case lengthPA# pa xs of
      n# -> PNested n# (replicatePA_Int# n# 1#) (upToPA_Int# n#) xs

singletonPA :: PA a -> (a :-> PArray a)
{-# INLINE singletonPA #-}
singletonPA pa = closure1 (singletonPA_v pa) (singletonPA_l pa)

mapPA_v :: PA a -> PA b -> (a :-> b) -> PArray a -> PArray b
{-# INLINE mapPA_v #-}
mapPA_v pa pb f as = replicatePA# (dPA_Clo pa pb) (lengthPA# pa as) f
                     $:^ as

mapPA_l :: PA a -> PA b
        -> PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
{-# INLINE mapPA_l #-}
mapPA_l pa pb fs (PNested n# lens idxs xs)
  = PNested n# lens idxs
            (replicatelPA# (dPA_Clo pa pb) (lengthPA# pa xs) lens fs $:^ xs)

mapPA :: PA a -> PA b -> ((a :-> b) :-> PArray a :-> PArray b)
{-# INLINE mapPA #-}
mapPA pa pb = closure2 (dPA_Clo pa pb) (mapPA_v pa pb) (mapPA_l pa pb)

crossMapPA_v :: PA a -> PA b -> PArray a -> (a :-> PArray b) -> PArray (a,b)
{-# INLINE crossMapPA_v #-}
crossMapPA_v pa pb as f
  = case lengthPA# pb bs of
      n# -> zipPA# pa pb (replicatelPA# pa n# lens as) bs
  where
    PNested _ lens _ bs = mapPA_v pa (dPA_PArray pb) f as

crossMapPA_l :: PA a -> PA b
             -> PArray (PArray a)
             -> PArray (a :-> PArray b)
             -> PArray (PArray (a,b))
{-# INLINE crossMapPA_l #-}
crossMapPA_l pa pb xss fs = error "crossMapPA_l"

crossMapPA :: PA a -> PA b -> (PArray a :-> (a :-> PArray b) :-> PArray (a,b))
{-# INLINE crossMapPA #-}
crossMapPA pa pb = closure2 (dPA_PArray pa) (crossMapPA_v pa pb)
                                            (crossMapPA_l pa pb)

zipPA_v :: PA a -> PA b -> PArray a -> PArray b -> PArray (a,b)
{-# INLINE zipPA_v #-}
zipPA_v pa pb xs ys = zipPA# pa pb xs ys

zipPA_l :: PA a -> PA b
        -> PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a,b))
{-# INLINE zipPA_l #-}
zipPA_l pa pb xss yss = error "zipPA_l"

zipPA :: PA a -> PA b -> (PArray a :-> PArray b :-> PArray (a,b))
{-# INLINE zipPA #-}
zipPA pa pb = closure2 (dPA_PArray pa) (zipPA_v pa pb) (zipPA_l pa pb)

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

packPA_v :: PA a -> PArray a -> PArray Bool -> PArray a
{-# INLINE packPA_v #-}
packPA_v pa xs bs = packPA# pa xs (truesPA# bs) (toPrimArrPA_Bool bs)

packPA_l :: PA a
         -> PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
{-# INLINE packPA_l #-}
packPA_l pa xss bss = error "packPA_l"

packPA :: PA a -> (PArray a :-> PArray Bool :-> PArray a)
{-# INLINE packPA #-}
packPA pa = closure2 (dPA_PArray pa) (packPA_v pa) (packPA_l pa)

filterPA_v :: PA a -> (a :-> Bool) -> PArray a -> PArray a
{-# INLINE filterPA_v #-}
filterPA_v pa p xs = packPA_v pa xs (mapPA_v pa dPA_Bool p xs)

filterPA_l :: PA a
           -> PArray (a :-> Bool) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE filterPA_l #-}
filterPA_l pa ps xss = error "filterPA_l"

filterPA :: PA a -> ((a :-> Bool) :-> PArray a :-> PArray a)
{-# INLINE filterPA #-}
filterPA pa = closure2 (dPA_Clo pa dPA_Bool) (filterPA_v pa) (filterPA_l pa)

indexPA_v :: PA a -> PArray a -> Int -> a
{-# INLINE indexPA_v #-}
indexPA_v pa xs (I# i#) = indexPA# pa xs i#

indexPA_l :: PA a -> PArray (PArray a) -> PArray Int -> PArray a
{-# INLINE indexPA_l #-}
indexPA_l pa (PNested _ lens idxs xs) (PInt _ is)
  = bpermutePA# pa xs (unsafe_zipWithPA_Int# (+) idxs is)

indexPA :: PA a -> (PArray a :-> Int :-> a)
{-# INLINE indexPA #-}
indexPA pa = closure2 (dPA_PArray pa) (indexPA_v pa) (indexPA_l pa)

