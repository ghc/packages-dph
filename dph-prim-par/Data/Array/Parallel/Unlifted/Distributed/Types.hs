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

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Distributed.Types (
  -- * Distributed types
  DT, Dist, MDist,

  -- * Operations on immutable distributed types
  indexD, unitD, zipD, unzipD, fstD, sndD, lengthD,
  newD,

  lengthUSegdD, lengthsUSegdD, indicesUSegdD, elementsUSegdD,

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
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Base

import Data.Word     (Word8)
import Control.Monad (liftM, liftM2, liftM3)

infixl 9 `indexD`

here s = "Data.Array.Parallel.Unlifted.Distributed.Types." ++ s

-- |Distributed types
-- ----------------------------

-- | Class of distributable types. Instances of 'DT' can be
-- distributed across all workers of a 'Gang'. All such types
-- must be hyperstrict as we do not want to pass thunks into distributed
-- computations.
class DT a where
  data Dist  a
  data MDist a :: * -> *

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

  -- | Number of elements in the distributed value. This is for debugging
  -- only.
  sizeD :: Dist a -> Int

  -- | Number of elements in the mutable distributed value. This is for
  -- debugging only.
  sizeMD :: MDist a s -> Int

-- Distributing hyperstrict types may not change their strictness.
instance (HS a, DT a) => HS (Dist a)

-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangD :: DT a => String -> Gang -> Dist a -> b -> b
checkGangD loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeD d) v

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
  data Dist ()    = DUnit  !Int
  data MDist () s = MDUnit !Int

  indexD  (DUnit n) i       = check (here "indexD[()]") n i $ ()
  newMD                     = return . MDUnit . gangSize
  readMD   (MDUnit n) i     = check (here "readMD[()]")  n i $
                               return ()
  writeMD  (MDUnit n) i ()  = check (here "writeMD[()]") n i $
                               return ()
  unsafeFreezeMD (MDUnit n) = return $ DUnit n

class UAE e => DPrim e where
  mkDPrim :: BUArr e -> Dist  e
  unDPrim :: Dist  e -> BUArr e

  mkMDPrim :: MBUArr s e -> MDist  e s
  unMDPrim :: MDist  e s -> MBUArr s e

primIndexD :: DPrim a => Dist a -> Int -> a
{-# INLINE primIndexD #-}
primIndexD = indexBU . unDPrim

primNewMD :: DPrim a => Gang -> ST s (MDist a s)
{-# INLINE primNewMD #-}
primNewMD = liftM mkMDPrim . newMBU . gangSize

primReadMD :: DPrim a => MDist a s -> Int -> ST s a
{-# INLINE primReadMD #-}
primReadMD = readMBU . unMDPrim

primWriteMD :: DPrim a => MDist a s -> Int -> a -> ST s ()
{-# INLINE primWriteMD #-}
primWriteMD = writeMBU . unMDPrim

primUnsafeFreezeMD :: DPrim a => MDist a s -> ST s (Dist a)
{-# INLINE primUnsafeFreezeMD #-}
primUnsafeFreezeMD = liftM mkDPrim . unsafeFreezeAllMBU . unMDPrim

primSizeD :: DPrim a => Dist a -> Int
{-# INLINE primSizeD #-}
primSizeD = lengthBU . unDPrim

primSizeMD :: DPrim a => MDist a s -> Int
{-# INLINE primSizeMD #-}
primSizeMD = lengthMBU . unMDPrim

instance DPrim Bool where
  mkDPrim           = DBool
  unDPrim (DBool a) = a

  mkMDPrim            = MDBool
  unMDPrim (MDBool a) = a

instance DT Bool where
  data Dist  Bool   = DBool  !(BUArr    Bool)
  data MDist Bool s = MDBool !(MBUArr s Bool)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

instance DPrim Char where
  mkDPrim           = DChar
  unDPrim (DChar a) = a

  mkMDPrim            = MDChar
  unMDPrim (MDChar a) = a

instance DT Char where
  data Dist  Char   = DChar  !(BUArr    Char)
  data MDist Char s = MDChar !(MBUArr s Char)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

instance DPrim Int where
  mkDPrim          = DInt
  unDPrim (DInt a) = a

  mkMDPrim            = MDInt
  unMDPrim (MDInt a) = a

instance DT Int where
  data Dist  Int   = DInt  !(BUArr    Int)
  data MDist Int s = MDInt !(MBUArr s Int)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

instance DPrim Word8 where
  mkDPrim            = DWord8
  unDPrim (DWord8 a) = a

  mkMDPrim             = MDWord8
  unMDPrim (MDWord8 a) = a

instance DT Word8 where
  data Dist  Word8   = DWord8  !(BUArr    Word8)
  data MDist Word8 s = MDWord8 !(MBUArr s Word8)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

instance DPrim Float where
  mkDPrim            = DFloat
  unDPrim (DFloat a) = a

  mkMDPrim             = MDFloat
  unMDPrim (MDFloat a) = a

instance DT Float where
  data Dist  Float   = DFloat  !(BUArr    Float)
  data MDist Float s = MDFloat !(MBUArr s Float)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

instance DPrim Double where
  mkDPrim             = DDouble
  unDPrim (DDouble a) = a

  mkMDPrim              = MDDouble
  unMDPrim (MDDouble a) = a

instance DT Double where
  data Dist  Double   = DDouble  !(BUArr    Double)
  data MDist Double s = MDDouble !(MBUArr s Double)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

instance (DT a, DT b) => DT (a :*: b) where
  data Dist  (a :*: b)   = DProd  !(Dist a)    !(Dist b)
  data MDist (a :*: b) s = MDProd !(MDist a s) !(MDist b s)

  indexD d i               = (fstD d `indexD` i) :*: (sndD d `indexD` i)
  newMD g                  = liftM2 MDProd (newMD g) (newMD g)
  readMD  (MDProd xs ys) i = liftM2 (:*:) (readMD xs i) (readMD ys i)
  writeMD (MDProd xs ys) i (x :*: y)
                            = writeMD xs i x >> writeMD ys i y
  unsafeFreezeMD (MDProd xs ys)
                            = liftM2 DProd (unsafeFreezeMD xs)
                                           (unsafeFreezeMD ys)
  sizeD  (DProd  x _) = sizeD  x
  sizeMD (MDProd x _) = sizeMD x

instance DT a => DT (MaybeS a) where
  data Dist  (MaybeS a)   = DMaybe  !(Dist  Bool)   !(Dist  a)
  data MDist (MaybeS a) s = MDMaybe !(MDist Bool s) !(MDist a s)

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
  sizeD  (DMaybe  b _) = sizeD  b
  sizeMD (MDMaybe b _) = sizeMD b

instance UA a => DT (UArr a) where
  data Dist  (UArr a)   = DUArr  !(Dist  Int)   !(BBArr    (UArr a))
  data MDist (UArr a) s = MDUArr !(MDist Int s) !(MBBArr s (UArr a))

  indexD (DUArr _ a) i = indexBB a i
  newMD g = liftM2 MDUArr (newMD g) (newMBB (gangSize g))
  readMD (MDUArr _ marr) = readMBB marr
  writeMD (MDUArr mlen marr) i a =
    do
      writeMD mlen i (lengthU a)
      writeMBB marr i a
  unsafeFreezeMD (MDUArr len a) = liftM2 DUArr (unsafeFreezeMD len)
                                                (unsafeFreezeAllMBB a)
  sizeD  (DUArr  _ a) = lengthBB  a
  sizeMD (MDUArr _ a) = lengthMBB a

instance DT USegd where
  data Dist  USegd   = DUSegd  !(Dist (UArr Int))
                               !(Dist (UArr Int))
                               !(Dist Int)
  data MDist USegd s = MDUSegd !(MDist (UArr Int) s)
                               !(MDist (UArr Int) s)
                               !(MDist Int        s)

  indexD (DUSegd lens idxs eles) i
          = mkUSegd (indexD lens i) (indexD idxs i) (indexD eles i)
  newMD g = liftM3 MDUSegd (newMD g) (newMD g) (newMD g)
  readMD (MDUSegd lens idxs eles) i
          = liftM3 mkUSegd (readMD lens i) (readMD idxs i) (readMD eles i)
  writeMD (MDUSegd lens idxs eles) i segd
          = do
              writeMD lens i (lengthsUSegd  segd)
              writeMD idxs i (indicesUSegd  segd)
              writeMD eles i (elementsUSegd segd)
  unsafeFreezeMD (MDUSegd lens idxs eles)
          = liftM3 DUSegd (unsafeFreezeMD lens)
                          (unsafeFreezeMD idxs)
                          (unsafeFreezeMD eles)
  sizeD  (DUSegd  _ _ eles) = sizeD eles
  sizeMD (MDUSegd _ _ eles) = sizeMD eles

lengthUSegdD :: Dist USegd -> Dist Int
{-# INLINE_DIST lengthUSegdD #-}
lengthUSegdD (DUSegd lens _ _) = lengthD lens

lengthsUSegdD :: Dist USegd -> Dist (UArr Int)
{-# INLINE_DIST lengthsUSegdD #-}
lengthsUSegdD (DUSegd lens _ _ ) = lens

indicesUSegdD :: Dist USegd -> Dist (UArr Int)
{-# INLINE_DIST indicesUSegdD #-}
indicesUSegdD (DUSegd _ idxs _) = idxs

elementsUSegdD :: Dist USegd -> Dist Int
{-# INLINE_DIST elementsUSegdD #-}
elementsUSegdD (DUSegd _ _ dns) = dns

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
zipD !x !y = checkEq (here "zipDT") "Size mismatch" (sizeD x) (sizeD y) $
             DProd x y

-- | Unpairing of distributed values.
unzipD :: (DT a, DT b) => Dist (a :*: b) -> Dist a :*: Dist b
{-# INLINE_DIST unzipD #-}
unzipD (DProd dx dy) = dx :*: dy

-- | Extract the first elements of a distributed pair.
fstD :: (DT a, DT b) => Dist (a :*: b) -> Dist a
{-# INLINE_DIST fstD #-}
fstD = fstS . unzipD

-- | Extract the second elements of a distributed pair.
sndD :: (DT a, DT b) => Dist (a :*: b) -> Dist b
{-# INLINE_DIST sndD #-}
sndD = sndS . unzipD

-- | Yield the distributed length of a distributed array.
lengthD :: UA a => Dist (UArr a) -> Dist Int
lengthD (DUArr l _) = l

