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
  -- * Immutable distributed types
  DT(..), Dist,

  -- * Operations on immutable distributed types
  zipDT, unzipDT, fstDT, sndDT,

  -- * Mutable distributed types
  MDT(..), MDist,

  -- * Distributed references
  DRef, dref, readDRef, writeDRef,

  -- * Assertions
  checkGangDT, checkGangMDT,

  -- * Debugging functions
  lengthDT, lengthMDT
) where

import Monad                                ( liftM, liftM2, zipWithM )
import Data.Array.Parallel.Distributed.Gang ( Gang, gangSize, gangST )
import Data.Array.Parallel.Base.Generics
import Data.Array.Parallel.Base.Prim
import Data.Array.Parallel.Base.BUArr
import Data.Array.Parallel.Base.Hyperstrict ( HS )
import Data.Array.Parallel.Base.Debug       ( check, checkEq )

-- |Immutable distributed types
-- ----------------------------

-- | Class of immutable distributed types. Instances of 'DT' can be
-- distributed across all workers of a 'Gang'. At the moment, all such types
-- must be hyperstrict as we do not want to pass thunks into distributed
-- computations. This may change if distributed computations themselves
-- becomes instances of 'DT'.
class HS a => DT a where
  -- data Dist a

  -- | Extract a single element of an immutable distributed value.
  indexDT  :: Dist a -> Int -> a

-- GADT TO REPLACE AT FOR THE MOMENT
data Dist a where
  DUnit_ :: !Int                   -> Dist ()
  DUnit  :: !Int                   -> Dist Unit
  DPrim  :: !(Prim a)              -> Dist a
  DProd  :: !(Dist a) -> !(Dist b) -> Dist (a :*: b)
  DDRef  :: !(MDist a s)           -> Dist (DRef s a)
--  DST    :: !Gang -> (Int -> ST s a) -> Dist (ST s a)
--  DFn    :: !Gang -> (Int -> a -> b) -> Dist (a -> b)

unDPrim :: Dist a -> BUArr a
unDPrim (DPrim p) = unPrim p

-- Distributing hyperstrict types may not change their strictness.
instance (HS a, DT a) => HS (Dist a)

-- | Number of elements in the distributed value. This is for debugging only
-- and not a method of 'DT'.
lengthDT :: Dist a -> Int
lengthDT (DUnit_ n)   = n
lengthDT (DUnit  n)   = n
lengthDT (DPrim  p)   = lengthBU (unPrim p)
lengthDT (DProd  x y) = lengthDT x
lengthDT (DDRef  md)  = lengthMDT md

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangDT :: DT a => String -> Gang -> Dist a -> b -> b
checkGangDT loc g d v = checkEq loc "Wrong gang" (gangSize g) (lengthDT d) v

-- Show instance (for debugging only)
instance (Show a, DT a) => Show (Dist a) where
  show d = show (map (indexDT d) [0 .. lengthDT d - 1])

-- | 'DT' instances
-- ----------------

instance DT () where
  indexDT  (DUnit_ n) i = check "Dist.indexDT[()]" n i $ ()

instance DT Unit where
  indexDT  (DUnit n) i = check "Dist.indexDT[Unit]" n i $ Unit

instance DT Bool where
  indexDT  = indexBU  . unDPrim

instance DT Char where
  indexDT  = indexBU  . unDPrim

instance DT Int where
  indexDT  = indexBU  . unDPrim

instance DT Float where
  indexDT  = indexBU  . unDPrim

instance DT Double where
  indexDT  = indexBU  . unDPrim

instance (DT a, DT b) => DT (a :*: b) where
  indexDT d i = (fstDT d `indexDT` i) :*: (sndDT d `indexDT` i)

-- | Operations on immutable distributed types
-- -------------------------------------------

-- | Pairing of distributed values.
-- /The two values must belong to the same 'Gang'./
zipDT :: (DT a, DT b) => Dist a -> Dist b -> Dist (a :*: b)
zipDT x y = checkEq "Dist.zipDT" "Size mismatch" (lengthDT x) (lengthDT y) $
            DProd x y

-- | Unpairing of distributed values.
unzipDT :: (DT a, DT b) => Dist (a :*: b) -> (Dist a, Dist b)
unzipDT (DProd dx dy) = (dx, dy)

-- | Extract the first elements of a distributed pair.
fstDT :: (DT a, DT b) => Dist (a :*: b) -> Dist a
fstDT = fst . unzipDT

-- | Extract the second elements of a distributed pair.
sndDT :: (DT a, DT b) => Dist (a :*: b) -> Dist b
sndDT = snd . unzipDT

-- | Mutable distributed types
-- ---------------------------

-- | Class of mutable distributed types. Note that all such types must be
-- hyperstrict as we do not want to return thunks from distributed
-- computations.
class DT a => MDT a where
  -- data MDist a s

  -- | Create an unitialised distributed value for the given 'Gang'.
  newMDT    :: Gang -> ST s (MDist a s)

  -- | Extract an element from a mutable distributed value.
  readMDT   :: MDist a s -> Int -> ST s a

  -- | Write an element of a mutable distributed value.
  writeMDT  :: MDist a s -> Int -> a -> ST s ()

  -- | Unsafely freeze a mutable distributed value.
  freezeMDT :: MDist a s -> ST s (Dist a)

-- GADT TO REPLACE AT FOR THE MOMENT
data MDist a s where
  MDUnit_ :: !Int                         -> MDist ()        s
  MDUnit  :: !Int                         -> MDist Unit      s
  MDPrim  :: !(MPrim a s)                 -> MDist a         s
  MDProd  :: !(MDist a s) -> !(MDist b s) -> MDist (a :*: b) s

unMDPrim :: MDist a s -> MBUArr s a
unMDPrim (MDPrim p) = unMPrim p

-- | Number of elements in the mutable distributed value. This is for debugging
-- only and is thus not a method of 'MDT'.
lengthMDT :: MDist a s -> Int
lengthMDT (MDUnit_ n)   = n
lengthMDT (MDUnit  n)   = n
lengthMDT (MDPrim  p)   = lengthMBU (unMPrim p)
lengthMDT (MDProd  x y) = lengthMDT x

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangMDT :: MDT a => String -> Gang -> MDist a s -> b -> b
checkGangMDT loc g d v = checkEq loc "Wrong gang" (gangSize g) (lengthMDT d) v

-- | MDT instances
-- ---------------

instance MDT () where
  newMDT                     = return . MDUnit_ . gangSize
  readMDT   (MDUnit_ n) i    = check "Dist.readMDT[()]" n i $
                               return ()
  writeMDT  (MDUnit_ n) i () = check "Dist.writeMDT[()]" n i $
                               return ()
  freezeMDT (MDUnit_ n)      = return $ DUnit_ n

instance MDT Unit where
  newMDT                      = return . MDUnit . gangSize
  readMDT   (MDUnit n) i      = check "Dist.readMDT[Unit]" n i $
                                return Unit
  writeMDT  (MDUnit n) i Unit = check "Dist.writeMDT[Unit]" n i $
                                return ()
  freezeMDT (MDUnit n)        = return $ DUnit n

instance MDT Bool where
  newMDT    = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT   = readMBU . unMDPrim
  writeMDT  = writeMBU . unMDPrim
  freezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance MDT Char where
  newMDT    = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT   = readMBU . unMDPrim
  writeMDT  = writeMBU . unMDPrim
  freezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance MDT Int where
  newMDT    = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT   = readMBU . unMDPrim
  writeMDT  = writeMBU . unMDPrim
  freezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance MDT Float where
  newMDT    = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT   = readMBU . unMDPrim
  writeMDT  = writeMBU . unMDPrim
  freezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance MDT Double where
  newMDT    = liftM (MDPrim . mkMPrim) . newMBU . gangSize
  readMDT   = readMBU . unMDPrim
  writeMDT  = writeMBU . unMDPrim
  freezeMDT = liftM (DPrim . mkPrim) . unsafeFreezeAllMBU . unMDPrim

instance (MDT a, MDT b) => MDT (a :*: b) where
  newMDT g                   = liftM2 MDProd (newMDT g) (newMDT g)
  readMDT   (MDProd xs ys) i = liftM2 (:*:) (readMDT xs i) (readMDT ys i)
  writeMDT  (MDProd xs ys) i (x :*: y) =
    do
      writeMDT xs i x
      writeMDT ys i y
  freezeMDT (MDProd xs ys)   = liftM2 DProd (freezeMDT xs) (freezeMDT ys)

-- | Mutable distributed references
--
-- Distributed references provide a mechanism for safely embedding mutable
-- distributed values into immutable ones. A mutable distributed value of type
-- @'MDist' a s@ can be turned into an immutable distributed reference of type
-- @'Dist' ('DRef' s a)@. A 'DRef' can be used to write and read a single
-- element of a mutable distributed value. The only way to obtain one is to
-- run a distributed computation on a distributed reference.
--
-- Note that 'DRef's are instances of 'DT' but not of 'MDT'. Thus, they can be
-- stored in immutable distributed values but not in mutable ones. This means
-- that a worker can never change /which/ value the 'DRef' points to
-- (although it can modify that value). The reasons for this are fairly
-- obvious - we do not want two workers modifying the same 'DRef' to point
-- to different distributed values as this would, in a way, break the data
-- parallel model.
--
-- Incidentially, this also ensures that we can never return 'DRef's from
-- 'Gang' computations. This is because results of such computations must be
-- instances of 'MDT' as they are written into mutable distributed values by
-- the primitives. This may or may not be useful.
--
-- /TODO:/ Move 'DRef's into a separate module once we have ATs.

-- | References to individual elements of a mutable distributed value.
data DRef s a = DRef !(MDist a s) !Int

-- | Yield a distributed reference for a mutable distributed value.
dref :: MDT a => MDist a s -> Dist (DRef s a)
dref = DDRef

-- | Read a reference.
readDRef :: MDT a => DRef s a -> ST s a
readDRef (DRef md i) = readMDT md i

-- | Write a reference.
writeDRef :: MDT a => DRef s a -> a -> ST s ()
writeDRef (DRef md i) = writeMDT md i

instance HS a => HS (DRef s a)

instance MDT a => DT (DRef s a) where
  indexDT  (DDRef mdt) = DRef mdt

{-
instance MDT a => DT (ST s a) where
  lengthDT (DST g st) = gangSize g
  indexDT  (DST g st) i = st i

instance (DT a, DT b) => DT (a -> b) where
  lengthDT (DFn g f)   = gangSize g
  indexDT  (DFn g f) i = f i

distST_ :: Dist (ST s ()) -> ST s ()
distST_ (DST g st) = gangST g st

distST :: MDT a => Dist (ST s a) -> ST s (Dist a)
distST (DST g st) = do
                      r <- newMDT g
                      distST_ . DST g $ \i -> st i >>= writeMDT r i
                      freezeMDT r
-}

