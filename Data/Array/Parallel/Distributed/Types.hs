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
  unitDT, zipDT, unzipDT, fstDT, sndDT, lengthsDT,

  -- * Mutable distributed types
  MDT(..), MDist,

  -- * Distributed references
  DRef, dref, readDRef, writeDRef,

  -- * Distributed computations
  DST, myDT, readMyMDT, writeMyMDT,
  runDistST_, runDistST,
  gangDST, runDST_, runDST,
  liftST,

  -- * Assertions
  checkGangDT, checkGangMDT,

  -- * Debugging functions
  sizeDT, sizeMDT
) where

import Monad                                ( liftM, liftM2, zipWithM )
import Data.Array.Parallel.Distributed.Gang ( Gang, gangSize, gangST )
import Data.Array.Parallel.Base.Prim
import Data.Array.Parallel.Base.BUArr
import Data.Array.Parallel.Base.BBArr
import Data.Array.Parallel.Base.Hyperstrict ( HS, (:*:)(..), fstS, sndS )
import Data.Array.Parallel.Monadic.UArr
import Data.Array.Parallel.Base.Debug       ( check, checkEq, uninitialised )

infixl 9 `indexDT`

here s = "Distributed.Types." ++ s

-- |Immutable distributed types
-- ----------------------------

-- | Class of immutable distributed types. Instances of 'DT' can be
-- distributed across all workers of a 'Gang'. At the moment, all such types
-- must be hyperstrict as we do not want to pass thunks into distributed
-- computations. This may change if distributed computations themselves
-- becomes instances of 'DT'.
class DT a where
  -- data Dist a

  -- | Extract a single element of an immutable distributed value.
  indexDT  :: Dist a -> Int -> a

-- GADT TO REPLACE AT FOR THE MOMENT
data Dist a where
  DUnit  :: !Int                   -> Dist ()
  DPrim  :: !(Prim a)              -> Dist a
  DProd  :: !(Dist a) -> !(Dist b) -> Dist (a :*: b)
  -- | Distributed arrays
  DUArr  :: !(Dist Int) -> !(BBArr (UArr a)) -> Dist (UArr a)
  -- | Distributed references
  DDRef  :: !(MDist a s)           -> Dist (DRef s a)
  -- | Distributed computations
  DistST :: !Gang -> !(DST s a) -> Dist (ST s a)

unDPrim :: Dist a -> BUArr a
unDPrim (DPrim p) = unPrim p

-- Distributing hyperstrict types may not change their strictness.
instance (HS a, DT a) => HS (Dist a)

-- | Number of elements in the distributed value. This is for debugging only
-- and not a method of 'DT'.
sizeDT :: Dist a -> Int
sizeDT (DUnit  n)   = n
sizeDT (DPrim  p)   = lengthBU (unPrim p)
sizeDT (DProd  x y) = sizeDT x
sizeDT (DUArr  _ a) = lengthBB a
sizeDT (DDRef  md)  = sizeMDT md
sizeDT (DistST g d) = gangSize g

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangDT :: DT a => String -> Gang -> Dist a -> b -> b
checkGangDT loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeDT d) v

-- Show instance (for debugging only)
instance (Show a, DT a) => Show (Dist a) where
  show d = show (map (indexDT d) [0 .. sizeDT d - 1])

-- | 'DT' instances
-- ----------------

instance DT () where
  indexDT  (DUnit n) i = check (here "indexDT[()]") n i $ ()

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

instance UA a => DT (UArr a) where
  indexDT (DUArr _ a) i = indexBB a i

-- | Operations on immutable distributed types
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

-- | Mutable distributed types
-- ---------------------------

-- | Class of mutable distributed types. Note that all such types must be
-- hyperstrict as we do not want to return thunks from distributed
-- computations.
class (DT a, HS a) => MDT a where
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
  MDUnit  :: !Int                         -> MDist ()          s
  MDPrim  :: !(MPrim a s)                 -> MDist a           s
  MDProd  :: !(MDist a s) -> !(MDist b s) -> MDist (a :*: b)   s
  MDUArr  :: !(MDist Int s) -> !(MBBArr s (UArr a)) -> MDist (UArr a)    s

unMDPrim :: MDist a s -> MBUArr s a
unMDPrim (MDPrim p) = unMPrim p

unMDUArr :: MDist (UArr a) s -> MBBArr s (UArr a)
unMDUArr (MDUArr _ marr) = marr

-- | Number of elements in the mutable distributed value. This is for debugging
-- only and is thus not a method of 'MDT'.
sizeMDT :: MDist a s -> Int
sizeMDT (MDUnit  n)    = n
sizeMDT (MDPrim  p)    = lengthMBU (unMPrim p)
sizeMDT (MDProd  x y)  = sizeMDT x
sizeMDT (MDUArr  _ ma) = lengthMBB ma

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangMDT :: MDT a => String -> Gang -> MDist a s -> b -> b
checkGangMDT loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeMDT d) v

-- | MDT instances
-- ---------------

instance MDT () where
  newMDT                    = return . MDUnit . gangSize
  readMDT   (MDUnit n) i    = check (here "readMDT[()]")  n i $
                              return ()
  writeMDT  (MDUnit n) i () = check (here "writeMDT[()]") n i $
                              return ()
  freezeMDT (MDUnit n)      = return $ DUnit n

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

instance UA a => MDT (UArr a) where
  newMDT g = liftM2 MDUArr (newMDT g)
                           (newMBB (gangSize g)
                                   (uninitialised $ here "newMDT[UArr a]"))
  readMDT  = readMBB  . unMDUArr
  writeMDT (MDUArr mlen marr) i a =
    do
      writeMDT mlen i (lengthU a)
      writeMBB marr i a
  freezeMDT (MDUArr len a) = liftM2 DUArr (freezeMDT len)
                                          (unsafeFreezeAllMBB a)

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

-- | Distributed computations.
--
-- Data-parallel computations of type 'DST' are data-parallel computations which
-- are run on each thread of a gang. At the moment, they can only access the
-- element of a (possibly mutable) distributed value owned by the current
-- thread. A 'DST' computation can be turned into a distributed 'ST'
-- computation by binding it to a specific gang. Note that 'DST s a' is
-- shapeless (i.e. can be run on any 'Gang') whereas 'Dist (ST s a)', like all
-- distributed values, is tied to a specific 'Gang'.
--
-- /TODO:/ Move this to a separate module once we have ATs.
--
-- /TODO:/ Add facilities for implementing parallel scans etc.
--
-- /TODO:/ Documentation.

-- | Data-parallel computations.
newtype DST s a = DST { unDST :: Int -> ST s a }

instance Monad (DST s) where
  return         = DST . const . return 
  DST p >>= f = DST $ \i -> do
                                    x <- p i
                                    unDST (f x) i

-- | Yields the index of the current thread within its gang.
dindex :: DST s Int
dindex = DST return

-- | Lifts an 'ST' computation into the 'DST' monad. The lifted computation
-- should be data parallel.
liftST :: ST s a -> DST s a
liftST p = DST $ \i -> p

-- | Yields the 'Dist' element owned by the current thread.
myDT :: DT a => Dist a -> DST s a
myDT dt = liftM (indexDT dt) dindex

-- | Yields the 'MDist' element owned by the current thread.
readMyMDT :: MDT a => MDist a s -> DST s a
readMyMDT mdt = do
                     i <- dindex
                     liftST $ readMDT mdt i

-- | Writes the 'MDist' element owned by the current thread.
writeMyMDT :: MDT a => MDist a s -> a -> DST s ()
writeMyMDT mdt x = do
                        i <- dindex
                        liftST $ writeMDT mdt i x

-- | Distributes a data-parallel computation over a 'Gang'.
gangDST :: Gang -> DST s a -> Dist (ST s a)
gangDST = DistST

-- | Runs a data-parallel computation on a 'Gang'. 
runDST_ :: Gang -> DST s () -> ST s ()
runDST_ g = runDistST_ . gangDST g

-- | Runs a data-parallel computation on a 'Gang', yielding the distributed
-- result.
runDST :: MDT a => Gang -> DST s a -> ST s (Dist a)
runDST g = runDistST . gangDST g

-- | Runs a distributed computation.
runDistST_ :: Dist (ST s ()) -> ST s ()
runDistST_ (DistST g p) = gangST g (unDST p)

-- | Runs a distributed computation, yielding the distributed result.
runDistST :: MDT a => Dist (ST s a) -> ST s (Dist a)
runDistST (DistST g p) =
  do
    mdt <- newMDT g
    runDistST_ . gangDST g $ writeMyMDT mdt =<< p
    freezeMDT mdt

instance DT (ST s a) where
  indexDT (DistST g p) i = check (here "indexDT[ST s a]") (gangSize g) i $
                           unDST p i

