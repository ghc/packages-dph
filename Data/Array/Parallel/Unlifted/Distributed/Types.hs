{-# OPTIONS -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed.Types
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Distributed types.
--

module Data.Array.Parallel.Unlifted.Distributed.Types (
  -- * Distributed types
  DT, Dist, MDist,

  -- * Operations on immutable distributed types
  indexD, unitD, zipD, unzipD, fstD, sndD, lengthD,
  newD, segdSD, concatSD,

  -- * Operations on mutable distributed types
  newMD, readMD, writeMD, unsafeFreezeMD,

  -- * Assertions
  checkGangD, checkGangMD,

  -- * Debugging functions
  sizeD, sizeMD
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize )
import Data.Array.Parallel.Arr
import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Unlifted.Segmented
import Data.Array.Parallel.Base

import Control.Monad (liftM, liftM2)

infixl 9 `indexD`

here s = "Data.Array.Parallel.Unlifted.Distributed.Types." ++ s

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
  -- The gang is used (only) to know how many elements are needed
  -- in the distributed value.
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
  DPrim  :: !(BUArr a)                       -> Dist a
  DProd  :: !(Dist a)   -> !(Dist b)         -> Dist (a :*: b)
  DMaybe :: !(Dist Bool) -> !(Dist a)        -> Dist (MaybeS a)

	-- The Dist Int redundantly records the size of the UArrs
	-- (redundantly because the UArrs also contain their sizes)
  DUArr  :: !(Dist Int) -> !(BBArr (UArr a)) -> Dist (UArr a)

  -- NOTE: comments here were de-haddockized by Waern, because 
  -- GADTS currently can't be documented
  DUSegd :: -- Local segment descriptors
            !(Dist (UArr (Int :*: Int)))
            -- Indicates whether the first local segment is
            -- split across two processors.
            -- -> !(Dist Bool)                
            -> Dist USegd

  DSUArr :: !(Dist USegd) -> !(Dist (UArr a)) -> Dist (SUArr a)

data MDist a s where
  MDUnit  :: !Int                                   -> MDist ()         s
  MDPrim  :: !(MBUArr s a)                          -> MDist a          s
  MDProd  :: !(MDist a s)   -> !(MDist b s)         -> MDist (a :*: b)  s
  MDMaybe :: !(MDist Bool s) -> !(MDist a s)        -> MDist (MaybeS a) s
  MDUArr  :: !(MDist Int s) -> !(MBBArr s (UArr a)) -> MDist (UArr a)   s

  MDUSegd :: !(MDist (UArr (Int :*: Int)) s)
          -- -> !(MDist Bool s)                        
                                                    -> MDist USegd      s

  MDSUArr :: !(MDist USegd s) -> !(MDist (UArr a) s) -> MDist (SUArr a) s

unDPrim :: UAE a => Dist a -> BUArr a
unDPrim (DPrim a) = a

unMDPrim :: UAE a => MDist a s -> MBUArr s a
unMDPrim (MDPrim ma) = ma

-- Distributing hyperstrict types may not change their strictness.
instance (HS a, DT a) => HS (Dist a)

-- | Number of elements in the distributed value. This is for debugging only
-- and not a method of 'DT'.
sizeD :: Dist a -> Int
sizeD (DUnit  n)   = n
sizeD (DPrim  a)   = lengthBU a
sizeD (DProd  x y) = sizeD x
sizeD (DUArr  _ a) = lengthBB a
sizeD (DMaybe b a) = sizeD b

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangD :: DT a => String -> Gang -> Dist a -> b -> b
checkGangD loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeD d) v

-- | Number of elements in the mutable distributed value. This is for debugging
-- only and is thus not a method of 'DT'.
sizeMD :: MDist a s -> Int
sizeMD (MDUnit  n)    = n
sizeMD (MDPrim  a)    = lengthMBU a
sizeMD (MDProd  x y)  = sizeMD x
sizeMD (MDUArr  _ ma) = lengthMBB ma
sizeMD (MDMaybe b a)  = sizeMD b

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

primIndexD :: UAE a => Dist a -> Int -> a
primIndexD = indexBU . unDPrim

primNewMD :: UAE a => Gang -> ST s (MDist a s)
primNewMD = liftM MDPrim . newMBU . gangSize

primReadMD :: UAE a => MDist a s -> Int -> ST s a
primReadMD = readMBU . unMDPrim

primWriteMD :: UAE a => MDist a s -> Int -> a -> ST s ()
primWriteMD = writeMBU . unMDPrim

primUnsafeFreezeMD :: UAE a => MDist a s -> ST s (Dist a)
primUnsafeFreezeMD = liftM DPrim . unsafeFreezeAllMBU . unMDPrim

instance DT Bool where
  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD

instance DT Char where
  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD

instance DT Int where
  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD

instance DT Float where
  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD

instance DT Double where
  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD

instance (DT a, DT b) => DT (a :*: b) where
  indexD d i               = (fstD d `indexD` i) :*: (sndD d `indexD` i)
  newMD g                  = liftM2 MDProd (newMD g) (newMD g)
  readMD  (MDProd xs ys) i = liftM2 (:*:) (readMD xs i) (readMD ys i)
  writeMD (MDProd xs ys) i (x :*: y)
                            = writeMD xs i x >> writeMD ys i y
  unsafeFreezeMD (MDProd xs ys)
                            = liftM2 DProd (unsafeFreezeMD xs)
                                           (unsafeFreezeMD ys)

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

instance DT USegd where
  indexD (DUSegd d) = toUSegd . indexD d
  newMD g = liftM MDUSegd (newMD g)
  readMD (MDUSegd d) = liftM toUSegd . readMD d
  writeMD (MDUSegd d) i segd = writeMD d i (fromUSegd segd)
  unsafeFreezeMD (MDUSegd d) = liftM DUSegd (unsafeFreezeMD d)

instance UA a => DT (SUArr a) where
  indexD (DSUArr dsegd da) i = indexD dsegd i >: indexD da i
  newMD g = liftM2 MDSUArr (newMD g) (newMD g)
  readMD (MDSUArr msegd ma) i = liftM2 (>:) (readMD msegd i)
                                            (readMD ma    i)
  writeMD (MDSUArr msegd ma) i sarr =
    do
      writeMD msegd i (segdSU sarr)
      writeMD ma    i (concatSU sarr)
  unsafeFreezeMD (MDSUArr msegd ma) = liftM2 DSUArr (unsafeFreezeMD msegd)
                                                    (unsafeFreezeMD ma)

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
-- /The two values must belong to the same/ 'Gang'.
zipD :: (DT a, DT b) => Dist a -> Dist b -> Dist (a :*: b)
{-# INLINE [1] zipD #-}
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

-- | Yield the distributed segment descriptor of a distributed segmented
-- array.
segdSD :: UA a => Dist (SUArr a) -> Dist USegd
segdSD (DSUArr dsegd _) = dsegd

-- | Flatten a distributed segmented array.
concatSD :: UA a => Dist (SUArr a) -> Dist (UArr a)
concatSD (DSUArr _ darr) = darr

