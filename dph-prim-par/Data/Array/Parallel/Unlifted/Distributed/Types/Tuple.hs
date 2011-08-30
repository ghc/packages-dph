{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Tuples
module Data.Array.Parallel.Unlifted.Distributed.Types.Tuple (
        -- * Pairs
        zipD, unzipD, fstD, sndD,
        
        -- * Triples
        zip3D, unzip3D
) where
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Data.Array.Parallel.Base
import Control.Monad

here s = "Data.Array.Parallel.Unlifted.Distributed.Types.Tuple." ++ s


-- Pairs ----------------------------------------------------------------------
instance (DT a, DT b) => DT (a,b) where
  data Dist  (a,b)   = DProd  !(Dist a)    !(Dist b)
  data MDist (a,b) s = MDProd !(MDist a s) !(MDist b s)

  indexD d i
   = (fstD d `indexD` i, sndD d `indexD` i)

  newMD g
   = liftM2 MDProd (newMD g) (newMD g)

  readMD  (MDProd xs ys) i
   = liftM2 (,) (readMD xs i) (readMD ys i)

  writeMD (MDProd xs ys) i (x,y)
   = do writeMD xs i x
        writeMD ys i y

  unsafeFreezeMD (MDProd xs ys)
   = liftM2 DProd (unsafeFreezeMD xs)
                  (unsafeFreezeMD ys)

  {-# INLINE deepSeqD #-}
  deepSeqD (x, y) z 
   = deepSeqD x (deepSeqD y z)

  sizeD  (DProd  x _) = sizeD  x
  sizeMD (MDProd x _) = sizeMD x

  measureD (x, y) 
   = "Pair " ++ "(" ++ measureD x ++ ") (" ++  measureD y ++ ")"


-- | Pairing of distributed values.
--   The two values must belong to the same 'Gang'.
zipD :: (DT a, DT b) => Dist a -> Dist b -> Dist (a,b)
{-# INLINE [0] zipD #-}
zipD !x !y 
        = checkEq (here "zipDT") "Size mismatch" (sizeD x) (sizeD y) 
        $ DProd x y

-- | Unpairing of distributed values.
unzipD :: (DT a, DT b) => Dist (a,b) -> (Dist a, Dist b)
{-# INLINE_DIST unzipD #-}
unzipD (DProd dx dy) = (dx,dy)


-- | Extract the first elements of a distributed pair.
fstD :: (DT a, DT b) => Dist (a,b) -> Dist a
{-# INLINE_DIST fstD #-}
fstD = fst . unzipD


-- | Extract the second elements of a distributed pair.
sndD :: (DT a, DT b) => Dist (a,b) -> Dist b
{-# INLINE_DIST sndD #-}
sndD = snd . unzipD


-- Triples --------------------------------------------------------------------
instance (DT a, DT b, DT c) => DT (a,b,c) where
  data Dist  (a,b,c)   = DProd3  !(Dist a)    !(Dist b)    !(Dist c)
  data MDist (a,b,c) s = MDProd3 !(MDist a s) !(MDist b s) !(MDist c s)

  indexD (DProd3 xs ys zs) i
   = ( xs `indexD` i
     , ys `indexD` i
     , zs `indexD` i)

  newMD g
   = liftM3 MDProd3 (newMD g) (newMD g) (newMD g)

  readMD  (MDProd3 xs ys zs) i
   = liftM3 (,,) (readMD xs i) (readMD ys i) (readMD zs i)

  writeMD (MDProd3 xs ys zs) i (x,y,z)
   = do writeMD xs i x
        writeMD ys i y
        writeMD zs i z

  unsafeFreezeMD (MDProd3 xs ys zs)
   = liftM3 DProd3 (unsafeFreezeMD xs)
                   (unsafeFreezeMD ys)
                   (unsafeFreezeMD zs)

  {-# INLINE deepSeqD #-}
  deepSeqD (x,y,z) k 
   = deepSeqD x (deepSeqD y (deepSeqD z k))

  sizeD  (DProd3  x _ _) = sizeD  x
  sizeMD (MDProd3 x _ _) = sizeMD x

  measureD (x,y,z)
   = "Triple " 
        ++ "(" ++ measureD x ++ ") "
        ++ "(" ++ measureD y ++ ") "
        ++ "(" ++ measureD z ++ ")"


-- | Pairing of distributed values.
-- /The two values must belong to the same/ 'Gang'.
zip3D   :: (DT a, DT b, DT c) => Dist a -> Dist b -> Dist c -> Dist (a,b,c)
{-# INLINE [0] zip3D #-}
zip3D !x !y !z
        = checkEq (here "zip3DT") "Size mismatch" (sizeD x) (sizeD y) 
        $ checkEq (here "zip3DT") "Size mismatch" (sizeD x) (sizeD z) 
        $ DProd3 x y z


-- | Unpairing of distributed values.
unzip3D  :: (DT a, DT b, DT c) => Dist (a,b,c) -> (Dist a, Dist b, Dist c)
{-# INLINE_DIST unzip3D #-}
unzip3D (DProd3 dx dy dz) = (dx,dy,dz)
