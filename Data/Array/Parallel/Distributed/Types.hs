{-# OPTIONS -fno-warn-incomplete-patterns #-}
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
  DT, Dist, MDist,

  -- * Operations on immutable distributed types
  indexD, unitD, zipD, unzipD, fstD, sndD, lengthD,
  newD,

  -- * Operations on mutable distributed types
  newMD, readMD, writeMD, unsafeFreezeMD,

  -- * Assertions
  checkGangD, checkGangMD,

  -- * Debugging functions
  sizeD, sizeMD
) where

import Monad                                ( liftM, liftM2 )
import Data.Array.Parallel.Distributed.Gang ( Gang, gangSize )
import Data.Array.Parallel.Arr.Prim
import Data.Array.Parallel.Arr.BUArr
import Data.Array.Parallel.Arr.BBArr
import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Base

infixl 9 `indexD`

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
  indexD         :: Dist a -> Int -> a

  -- | Create an unitialised distributed value for the given 'Gang'.
  newMD          :: Gang                  -> ST s (MDist a s)

  -- | Extract an element from a mutable distributed value.
  readMD         :: MDist a s -> Int      -> ST s a

  -- | Write an element of a mutable distributed value.
  writeMD        :: MDist a s -> Int -> a -> ST s ()

  -- | Unsafely freeze a mutable distributed value.
  unsafeFreezeMD :: MDist a s             -> ST s (Dist a)

-- GADTs TO REPLACE ATs FOR THE MOMENT
data Dist a where
  DUnit  :: !Int                             -> Dist ()
  DPrim  :: !(Prim a)                        -> Dist a
  DProd  :: !(Dist a)   -> !(Dist b)         -> Dist (a :*: b)
  DUArr  :: !(Dist Int) -> !(BBArr (UArr a)) -> Dist (UArr a)
  DMaybe :: !(Dist Bool) -> !(Dist a)        -> Dist (MaybeS a)

data MDist a s where
  MDUnit  :: !Int                                   -> MDist ()        s
  MDPrim  :: !(MPrim a s)                           -> MDist a         s
  MDProd  :: !(MDist a s)   -> !(MDist b s)         -> MDist (a :*: b) s
  MDUArr  :: !(MDist Int s) -> !(MBBArr s (UArr a)) -> MDist (UArr a)  s
  MDMaybe :: !(MDist Bool s) -> !(MDist a s)        -> MDist (MaybeS a) s

unDPrim :: Dist a -> BUArr a
unDPrim (DPrim p) = unPrim p

unMDPrim :: MDist a s -> MBUArr s a
unMDPrim (MDPrim p) = unMPrim p

-- Distributing hyperstrict types may not change their strictness.
instance (HS a, DT a) => HS (Dist a)

-- | Number of elements in the distributed value. This is for debugging only
-- and not a method of 'DT'.
sizeD :: Dist a -> Int
sizeD (DUnit  n)   = n
sizeD (DPrim  p)   = lengthBU (unPrim p)
sizeD (DProd  x y) = sizeD x
sizeD (DUArr  _ a) = lengthBB a

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangD :: DT a => String -> Gang -> Dist a -> b -> b
checkGangD loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeD d) v

-- | Number of elements in the mutable distributed value. This is for debugging
-- only and is thus not a method of 'DT'.
sizeMD :: MDist a s -> Int
sizeMD (MDUnit  n)    = n
sizeMD (MDPrim  p)    = lengthMBU (unMPrim p)
sizeMD (MDProd  x y)  = sizeMD x
sizeMD (MDUArr  _ ma) = lengthMBB ma

-- | Check that the sizes of the 'Gang' and of the mutable distributed value
-- match.
checkGangMD :: DT a => String -> Gang -> MDist a s -> b -> b
checkGangMD loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeMD d) v

-- Show instance (for debugging only)
instance (Show a, DT a) => Show (Dist a) where
  show d = show (map (indexD d) [0 .. sizeD d - 1])

-- | 'DT' instances
-- ----------------

instance DT () where
  indexD  (DUnit n) i       = check (here "indexD[()]") n i $ ()
  newMD                     = return . MDUnit . gangSize
  readMD   (MDUnit n) i     = check (here "readMD[()]")  n i $
                               return ()
  writeMD  (MDUnit n) i ()  = check (here "writeMD[()]") n i $
                               return ()
  unsafeFreezeMD (MDUnit n) = return $ DUnit n

instance DT Bool where
  indexD         = indexBU . unDPrim
  newMD          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMD         = readMBU . unMDPrim
  writeMD        = writeMBU . unMDPrim
  unsafeFreezeMD = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Char where
  indexD         = indexBU . unDPrim
  newMD          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMD         = readMBU . unMDPrim
  writeMD        = writeMBU . unMDPrim
  unsafeFreezeMD = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Int where
  indexD         = indexBU . unDPrim
  newMD          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMD         = readMBU . unMDPrim
  writeMD        = writeMBU . unMDPrim
  unsafeFreezeMD = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Float where
  indexD         = indexBU . unDPrim
  newMD          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMD         = readMBU . unMDPrim
  writeMD        = writeMBU . unMDPrim
  unsafeFreezeMD = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance DT Double where
  indexD         = indexBU  . unDPrim
  newMD          = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMD         = readMBU . unMDPrim
  writeMD        = writeMBU . unMDPrim
  unsafeFreezeMD = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance (DT a, DT b) => DT (a :*: b) where
  indexD d i               = (fstD d `indexD` i) :*: (sndD d `indexD` i)
  newMD g                  = liftM2 MDProd (newMD g) (newMD g)
  readMD  (MDProd xs ys) i = liftM2 (:*:) (readMD xs i) (readMD ys i)
  writeMD (MDProd xs ys) i (x :*: y)
                            = writeMD xs i x >> writeMD ys i y
  unsafeFreezeMD (MDProd xs ys)
                            = liftM2 DProd (unsafeFreezeMD xs)
                                           (unsafeFreezeMD ys)

instance UA a => DT (UArr a) where
  indexD (DUArr _ a) i = indexBB a i
  newMD g = liftM2 MDUArr (newMD g) (newMBB (gangSize g))
  readMD (MDUArr _ marr) = readMBB marr
  writeMD (MDUArr mlen marr) i a =
    do
      writeMD mlen i (lengthU a)
      writeMBB marr i a
  unsafeFreezeMD (MDUArr len a) = liftM2 DUArr (unsafeFreezeMD len)
                                                (unsafeFreezeAllMBB a)

instance DT a => DT (MaybeS a) where
  indexD (DMaybe bs as) i
    | bs `indexD` i       = JustS $ as `indexD` i
    | otherwise           = NothingS
  newMD g = liftM2 MDMaybe (newMD g) (newMD g)
  readMD (MDMaybe bs as) i =
    do
      b <- readMD bs i
      if b then liftM JustS $ readMD as i
           else return NothingS
  writeMD (MDMaybe bs as) i NothingS  = writeMD bs i False
  writeMD (MDMaybe bs as) i (JustS x) = writeMD bs i True
                                     >> writeMD as i x
  unsafeFreezeMD (MDMaybe bs as) = liftM2 DMaybe (unsafeFreezeMD bs)
                                                 (unsafeFreezeMD as)

-- |Basic operations on immutable distributed types
-- -------------------------------------------

newD :: DT a => Gang -> (forall s . MDist a s -> ST s ()) -> Dist a
newD g init =
  runST (do
           mdt <- newMD g
           init mdt
           unsafeFreezeMD mdt)
                    

-- | Yield a distributed unit.
unitD :: Gang -> Dist ()
unitD = DUnit . gangSize

-- | Pairing of distributed values.
-- /The two values must belong to the same 'Gang'./
zipD :: (DT a, DT b) => Dist a -> Dist b -> Dist (a :*: b)
zipD x y = checkEq (here "zipDT") "Size mismatch" (sizeD x) (sizeD y) $
            DProd x y

-- | Unpairing of distributed values.
unzipD :: (DT a, DT b) => Dist (a :*: b) -> Dist a :*: Dist b
unzipD (DProd dx dy) = dx :*: dy

-- | Extract the first elements of a distributed pair.
fstD :: (DT a, DT b) => Dist (a :*: b) -> Dist a
fstD = fstS . unzipD

-- | Extract the second elements of a distributed pair.
sndD :: (DT a, DT b) => Dist (a :*: b) -> Dist b
sndD = sndS . unzipD

-- | Yield the distributed length of a distributed array.
lengthD :: UA a => Dist (UArr a) -> Dist Int
lengthD (DUArr l _) = l

