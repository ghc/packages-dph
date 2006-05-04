-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed.Types
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Distributed types.
--

module Data.Array.Parallel.Distributed.Types (
  -- * Distributed types
  DT(..), Dist, MDist,

  -- * Operations on immutable distributed types
  unitDT, zipDT, unzipDT, fstDT, sndDT, lengthsDT,

  -- * Assertions
  checkGangDT, checkGangMDT,

  -- * Debugging functions
  sizeDT, sizeMDT
) where

import Monad                                ( liftM, liftM2 )
import Data.Array.Parallel.Distributed.Gang ( Gang, gangSize )
import Data.Array.Parallel.Arr.Prim
import Data.Array.Parallel.Arr.BUArr
import Data.Array.Parallel.Arr.BBArr
import Data.Array.Parallel.Unlifted.Flat.UArr
import Data.Array.Parallel.Base

infixl 9 `indexDT`

here s = "Distributed.Types." ++ s

-- |Distributed types
-- ----------------------------

-- | Class of distributable types. Instances of 'DT' can be
-- distributed across all workers of a 'Gang'. All such types
-- must be hyperstrict as we do not want to pass thunks into distributed
-- computations.
class DT a where
  -- data Dist a
  -- data MDist a s

  -- | Extract a single element of an immutable distributed value.
  indexDT         :: Dist a -> Int -> a

  -- | Create an unitialised distributed value for the given 'Gang'.
  newMDT          :: Gang                  -> ST s (MDist a s)

  -- | Extract an element from a mutable distributed value.
  readMDT         :: MDist a s -> Int      -> ST s a

  -- | Write an element of a mutable distributed value.
  writeMDT        :: MDist a s -> Int -> a -> ST s ()

  -- | Unsafely freeze a mutable distributed value.
  unsafeFreezeMDT :: MDist a s             -> ST s (Dist a)

-- GADTs TO REPLACE ATs FOR THE MOMENT
data Dist a where
  DUnit  :: !Int                             -> Dist ()
  DPrim  :: !(Prim a)                        -> Dist a
  DProd  :: !(Dist a)   -> !(Dist b)         -> Dist (a :*: b)
  DUArr  :: !(Dist Int) -> !(BBArr (UArr a)) -> Dist (UArr a)

data MDist a s where
  MDUnit  :: !Int                                   -> MDist ()        s
  MDPrim  :: !(MPrim a s)                           -> MDist a         s
  MDProd  :: !(MDist a s)   -> !(MDist b s)         -> MDist (a :*: b) s
  MDUArr  :: !(MDist Int s) -> !(MBBArr s (UArr a)) -> MDist (UArr a)  s

unDPrim :: Dist a -> BUArr a
unDPrim (DPrim p) = unPrim p

unMDPrim :: MDist a s -> MBUArr s a
unMDPrim (MDPrim p) = unMPrim p

-- Distributing hyperstrict types may not change their strictness.
instance (HS a, DT a) => HS (Dist a)

-- | Number of elements in the distributed value. This is for debugging only
-- and not a method of 'DT'.
sizeDT :: Dist a -> Int
sizeDT (DUnit  n)   = n
sizeDT (DPrim  p)   = lengthBU (unPrim p)
sizeDT (DProd  x y) = sizeDT x
sizeDT (DUArr  _ a) = lengthBB a

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangDT :: DT a => String -> Gang -> Dist a -> b -> b
checkGangDT loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeDT d) v

-- | Number of elements in the mutable distributed value. This is for debugging
-- only and is thus not a method of 'DT'.
sizeMDT :: MDist a s -> Int
sizeMDT (MDUnit  n)    = n
sizeMDT (MDPrim  p)    = lengthMBU (unMPrim p)
sizeMDT (MDProd  x y)  = sizeMDT x
sizeMDT (MDUArr  _ ma) = lengthMBB ma

-- | Check that the sizes of the 'Gang' and of the mutable distributed value
-- match.
checkGangMDT :: DT a => String -> Gang -> MDist a s -> b -> b
checkGangMDT loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeMDT d) v

-- Show instance (for debugging only)
instance (Show a, DT a) => Show (Dist a) where
  show d = show (map (indexDT d) [0 .. sizeDT d - 1])

-- | 'DT' instances
-- ----------------

instance DT () where
  indexDT  (DUnit n) i       = check (here "indexDT[()]") n i $ ()
  newMDT                     = return . MDUnit . gangSize
  readMDT   (MDUnit n) i     = check (here "readMDT[()]")  n i $
                               return ()
  writeMDT  (MDUnit n) i ()  = check (here "writeMDT[()]") n i $
                               return ()
  unsafeFreezeMDT (MDUnit n) = return $ DUnit n

instance DT Bool where
  indexDT         = indexBU . unDPrim
  newMDT          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT         = readMBU . unMDPrim
  writeMDT        = writeMBU . unMDPrim
  unsafeFreezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Char where
  indexDT         = indexBU . unDPrim
  newMDT          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT         = readMBU . unMDPrim
  writeMDT        = writeMBU . unMDPrim
  unsafeFreezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Int where
  indexDT         = indexBU . unDPrim
  newMDT          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT         = readMBU . unMDPrim
  writeMDT        = writeMBU . unMDPrim
  unsafeFreezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Float where
  indexDT         = indexBU . unDPrim
  newMDT          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT         = readMBU . unMDPrim
  writeMDT        = writeMBU . unMDPrim
  unsafeFreezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Double where
  indexDT         = indexBU  . unDPrim
  newMDT          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT         = readMBU . unMDPrim
  writeMDT        = writeMBU . unMDPrim
  unsafeFreezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance (DT a, DT b) => DT (a :*: b) where
  indexDT d i               = (fstDT d `indexDT` i) :*: (sndDT d `indexDT` i)
  newMDT g                  = liftM2 MDProd (newMDT g) (newMDT g)
  readMDT  (MDProd xs ys) i = liftM2 (:*:) (readMDT xs i) (readMDT ys i)
  writeMDT (MDProd xs ys) i (x :*: y)
                            = writeMDT xs i x >> writeMDT ys i y
  unsafeFreezeMDT (MDProd xs ys)
                            = liftM2 DProd (unsafeFreezeMDT xs)
                                           (unsafeFreezeMDT ys)

instance UA a => DT (UArr a) where
  indexDT (DUArr _ a) i = indexBB a i
  newMDT g = liftM2 MDUArr (newMDT g)
                           (newMBB (gangSize g)
                                   (uninitialised $ here "newMDT[UArr a]"))
  readMDT (MDUArr _ marr) = readMBB marr
  writeMDT (MDUArr mlen marr) i a =
    do
      writeMDT mlen i (lengthU a)
      writeMBB marr i a
  unsafeFreezeMDT (MDUArr len a) = liftM2 DUArr (unsafeFreezeMDT len)
                                                (unsafeFreezeAllMBB a)

-- |Basic operations on immutable distributed types
-- -------------------------------------------

-- | Yield a distributed unit.
unitDT :: Gang -> Dist ()
unitDT = DUnit . gangSize

-- | Pairing of distributed values.
-- /The two values must belong to the same 'Gang'./
zipDT :: (DT a, DT b) => Dist a -> Dist b -> Dist (a :*: b)
zipDT x y = checkEq (here "zipDT") "Size mismatch" (sizeDT x) (sizeDT y) $
            DProd x y

-- | Unpairing of distributed values.
unzipDT :: (DT a, DT b) => Dist (a :*: b) -> Dist a :*: Dist b
unzipDT (DProd dx dy) = dx :*: dy

-- | Extract the first elements of a distributed pair.
fstDT :: (DT a, DT b) => Dist (a :*: b) -> Dist a
fstDT = fstS . unzipDT

-- | Extract the second elements of a distributed pair.
sndDT :: (DT a, DT b) => Dist (a :*: b) -> Dist b
sndDT = sndS . unzipDT

-- | Yield the distributed length of a distributed array.
lengthsDT :: UA a => Dist (UArr a) -> Dist Int
lengthsDT (DUArr l _) = l

