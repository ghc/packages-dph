{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Types (
  -- * Distributed types
  DT, Dist, MDist, DPrim(..),

  -- * Operations on immutable distributed types
  indexD, unitD, zipD, unzipD, fstD, sndD, lengthD,
  newD,
  -- zipSD, unzipSD, fstSD, sndSD,
  deepSeqD,

  lengthUSegdD, lengthsUSegdD, indicesUSegdD, elementsUSegdD,

  -- * Operations on mutable distributed types
  newMD, readMD, writeMD, unsafeFreezeMD,

  -- * Assertions
  checkGangD, checkGangMD,

  -- * Debugging functions
  sizeD, sizeMD, measureD, debugD
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize )
import Data.Array.Parallel.Unlifted.Sequential.Vector ( Unbox, Vector )
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Base

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as MBV

import Data.Word     (Word8)
import Control.Monad (liftM, liftM2, liftM3)

import Data.List ( intercalate )

infixl 9 `indexD`

here s = "Data.Array.Parallel.Unlifted.Distributed.Types." ++ s


-- Distributed Types ----------------------------------------------------------
-- | Class of distributable types. Instances of 'DT' can be
--   distributed across all workers of a 'Gang'. 
--   All such types must be hyperstrict as we do not want to pass thunks
--   into distributed computations.
class DT a where
  data Dist  a
  data MDist a :: * -> *

  -- | Extract a single element of an immutable distributed value.
  indexD         :: Dist a -> Int -> a

  -- | Create an unitialised distributed value for the given 'Gang'.
  --   The gang is used (only) to know how many elements are needed
  --   in the distributed value.
  newMD          :: Gang                  -> ST s (MDist a s)

  -- | Extract an element from a mutable distributed value.
  readMD         :: MDist a s -> Int      -> ST s a

  -- | Write an element of a mutable distributed value.
  writeMD        :: MDist a s -> Int -> a -> ST s ()

  -- | Unsafely freeze a mutable distributed value.
  unsafeFreezeMD :: MDist a s             -> ST s (Dist a)

  deepSeqD       :: a -> b -> b
  deepSeqD = seq


  -- Debugging ------------------------
  -- | Number of elements in the distributed value.
  --   For debugging only, as we shouldn't depend on the size of the gang.
  sizeD :: Dist a -> Int

  -- | Number of elements in the mutable distributed value.
  --   For debugging only, as we shouldn't care about the actual number.
  sizeMD :: MDist a s -> Int

  -- | Show a distributed value.
  --   For debugging only.
  measureD :: a -> String
  measureD _ = "None"

-- Show instance (for debugging only)
instance (Show a, DT a) => Show (Dist a) where
  show d = show (Prelude.map (indexD d) [0 .. sizeD d - 1])



-- Checking -------------------------------------------------------------------
-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangD :: DT a => String -> Gang -> Dist a -> b -> b
checkGangD loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeD d) v


-- | Check that the sizes of the 'Gang' and of the mutable distributed value match.
checkGangMD :: DT a => String -> Gang -> MDist a s -> b -> b
checkGangMD loc g d v = checkEq loc "Wrong gang" (gangSize g) (sizeMD d) v


-- Operations -----------------------------------------------------------------
-- | Given a computation that can write its result to a mutable distributed value, 
--   run the computation to generate an immutable distributed value.
newD :: DT a => Gang -> (forall s . MDist a s -> ST s ()) -> Dist a
newD g init =
  runST (do
           mdt <- newMD g
           init mdt
           unsafeFreezeMD mdt)

-- | Show all members of a distributed value.
debugD :: DT a => Dist a -> String
debugD d = "["
         ++ intercalate "," [measureD (indexD d i) | i <- [0 .. sizeD d-1]]
         ++ "]"


-- DPrim ----------------------------------------------------------------------
-- | For distributed primitive values, we can just store all the members in
--   a vector. The vector has the same length as the number of threads in the gang.
--
class Unbox e => DPrim e where

  -- | Make an immutable distributed value.
  mkDPrim :: V.Vector e -> Dist  e

  -- | Unpack an immutable distributed value back into a vector.
  unDPrim :: Dist  e -> V.Vector e

  -- | Make a mutable distributed value.
  mkMDPrim :: MV.STVector s e -> MDist  e s

  -- | Unpack a mutable distributed value back into a vector.
  unMDPrim :: MDist  e s -> MV.STVector s e


-- | Get the member corresponding to a thread index.
primIndexD :: DPrim a => Dist a -> Int -> a
{-# INLINE primIndexD #-}
primIndexD = (V.!) . unDPrim


-- | Create a new distributed value, having as many members as threads
--   in the given 'Gang'.
primNewMD :: DPrim a => Gang -> ST s (MDist a s)
{-# INLINE primNewMD #-}
primNewMD = liftM mkMDPrim . MV.new . gangSize


-- | Read the member of a distributed value corresponding to the given thread index.
primReadMD :: DPrim a => MDist a s -> Int -> ST s a
{-# INLINE primReadMD #-}
primReadMD = MV.read . unMDPrim


-- | Write the member of a distributed value corresponding to the given thread index.
primWriteMD :: DPrim a => MDist a s -> Int -> a -> ST s ()
{-# INLINE primWriteMD #-}
primWriteMD = MV.write . unMDPrim


-- | Freeze a mutable distributed value to an immutable one.
--   You promise not to update the mutable one any further.
primUnsafeFreezeMD :: DPrim a => MDist a s -> ST s (Dist a)
{-# INLINE primUnsafeFreezeMD #-}
primUnsafeFreezeMD = liftM mkDPrim . V.unsafeFreeze . unMDPrim


-- | Get the size of a distributed value, that is, the number of threads
--   in the gang that it was created for.
primSizeD :: DPrim a => Dist a -> Int
{-# INLINE primSizeD #-}
primSizeD = V.length . unDPrim


-- | Get the size of a distributed mutable value, that is, the number of threads
--   in the gang it was created for.
primSizeMD :: DPrim a => MDist a s -> Int
{-# INLINE primSizeMD #-}
primSizeMD = MV.length . unMDPrim


-- Unit -----------------------------------------------------------------------
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

  sizeD  = error "dph-prim-par:sizeD[()] undefined"
  sizeMD = error "dph-prim-par:sizeMD[()] undefined"

-- | Yield a distributed unit.
unitD :: Gang -> Dist ()
{-# INLINE_DIST unitD #-}
unitD = DUnit . gangSize


-- Bool -----------------------------------------------------------------------
instance DPrim Bool where
  mkDPrim           = DBool
  unDPrim (DBool a) = a

  mkMDPrim            = MDBool
  unMDPrim (MDBool a) = a

instance DT Bool where
  data Dist  Bool   = DBool  !(V.Vector    Bool)
  data MDist Bool s = MDBool !(MV.STVector s Bool)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD


-- Char -----------------------------------------------------------------------
instance DPrim Char where
  mkDPrim           = DChar
  unDPrim (DChar a) = a

  mkMDPrim            = MDChar
  unMDPrim (MDChar a) = a

instance DT Char where
  data Dist  Char   = DChar  !(V.Vector    Char)
  data MDist Char s = MDChar !(MV.STVector s Char)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD


-- Int ------------------------------------------------------------------------
instance DPrim Int where
  mkDPrim          = DInt
  unDPrim (DInt a) = a

  mkMDPrim            = MDInt
  unMDPrim (MDInt a) = a

instance DT Int where
  data Dist  Int   = DInt  !(V.Vector    Int)
  data MDist Int s = MDInt !(MV.STVector s Int)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD

  measureD n = "Int " ++ show n


-- Word8 ----------------------------------------------------------------------
instance DPrim Word8 where
  mkDPrim            = DWord8
  unDPrim (DWord8 a) = a

  mkMDPrim             = MDWord8
  unMDPrim (MDWord8 a) = a

instance DT Word8 where
  data Dist  Word8   = DWord8  !(V.Vector    Word8)
  data MDist Word8 s = MDWord8 !(MV.STVector s Word8)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD


-- Float ----------------------------------------------------------------------
instance DPrim Float where
  mkDPrim            = DFloat
  unDPrim (DFloat a) = a

  mkMDPrim             = MDFloat
  unMDPrim (MDFloat a) = a

instance DT Float where
  data Dist  Float   = DFloat  !(V.Vector    Float)
  data MDist Float s = MDFloat !(MV.STVector s Float)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD


-- Double ---------------------------------------------------------------------
instance DPrim Double where
  mkDPrim             = DDouble
  unDPrim (DDouble a) = a

  mkMDPrim              = MDDouble
  unMDPrim (MDDouble a) = a

instance DT Double where
  data Dist  Double   = DDouble  !(V.Vector    Double)
  data MDist Double s = MDDouble !(MV.STVector s Double)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD


-- Pairs ----------------------------------------------------------------------
instance (DT a, DT b) => DT (a,b) where
  data Dist  (a,b)   = DProd  !(Dist a)    !(Dist b)
  data MDist (a,b) s = MDProd !(MDist a s) !(MDist b s)

  indexD d i               = (fstD d `indexD` i,sndD d `indexD` i)
  newMD g                  = liftM2 MDProd (newMD g) (newMD g)
  readMD  (MDProd xs ys) i = liftM2 (,) (readMD xs i) (readMD ys i)
  writeMD (MDProd xs ys) i (x,y)
                            = writeMD xs i x >> writeMD ys i y
  unsafeFreezeMD (MDProd xs ys)
                            = liftM2 DProd (unsafeFreezeMD xs)
                                           (unsafeFreezeMD ys)

  {-# INLINE deepSeqD #-}
  deepSeqD (x,y) z = deepSeqD x (deepSeqD y z)

  sizeD  (DProd  x _) = sizeD  x
  sizeMD (MDProd x _) = sizeMD x

  measureD (x,y) = "Pair " ++ "(" ++ measureD x ++ ") (" ++  measureD y ++ ")"


-- | Pairing of distributed values.
-- /The two values must belong to the same/ 'Gang'.
zipD :: (DT a, DT b) => Dist a -> Dist b -> Dist (a,b)
{-# INLINE [0] zipD #-}
zipD !x !y = checkEq (here "zipDT") "Size mismatch" (sizeD x) (sizeD y) $
             DProd x y

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


-- Maybe ----------------------------------------------------------------------
instance DT a => DT (Maybe a) where
  data Dist  (Maybe a)   = DMaybe  !(Dist  Bool)   !(Dist  a)
  data MDist (Maybe a) s = MDMaybe !(MDist Bool s) !(MDist a s)

  indexD (DMaybe bs as) i
    | bs `indexD` i       = Just $ as `indexD` i
    | otherwise           = Nothing
  newMD g = liftM2 MDMaybe (newMD g) (newMD g)
  readMD (MDMaybe bs as) i =
    do
      b <- readMD bs i
      if b then liftM Just $ readMD as i
           else return Nothing
  writeMD (MDMaybe bs as) i Nothing  = writeMD bs i False
  writeMD (MDMaybe bs as) i (Just x) = writeMD bs i True
                                     >> writeMD as i x
  unsafeFreezeMD (MDMaybe bs as) = liftM2 DMaybe (unsafeFreezeMD bs)
                                                 (unsafeFreezeMD as)

  {-# INLINE deepSeqD #-}
  deepSeqD Nothing  z = z
  deepSeqD (Just x) z = deepSeqD x z

  sizeD  (DMaybe  b _) = sizeD  b
  sizeMD (MDMaybe b _) = sizeMD b

  measureD Nothing = "Nothing"
  measureD (Just x) = "Just (" ++ measureD x ++ ")"


-- Vector ---------------------------------------------------------------------
instance Unbox a => DT (V.Vector a) where
  data Dist  (Vector a)   = DVector  !(Dist  Int)   !(BV.Vector      (Vector a))
  data MDist (Vector a) s = MDVector !(MDist Int s) !(MBV.STVector s (Vector a))

  indexD (DVector _ a) i = a BV.! i
  newMD g = liftM2 MDVector (newMD g) (MBV.replicate (gangSize g)
                                         (error "MDist (Vector a) - uninitalised"))
  readMD (MDVector _ marr) = MBV.read marr
  writeMD (MDVector mlen marr) i a =
    do
      writeMD mlen i (V.length a)
      MBV.write marr i $! a
  unsafeFreezeMD (MDVector len a) = liftM2 DVector (unsafeFreezeMD len)
                                               (BV.unsafeFreeze a)
  sizeD  (DVector  _ a) = BV.length  a
  sizeMD (MDVector _ a) = MBV.length a

  measureD xs = "Vector " ++ show (V.length xs)


-- | Yield the distributed length of a distributed array.
lengthD :: Unbox a => Dist (Vector a) -> Dist Int
lengthD (DVector l _) = l


-- USegd ----------------------------------------------------------------------
instance DT USegd where
  data Dist  USegd   = DUSegd  !(Dist (Vector Int))
                               !(Dist (Vector Int))
                               !(Dist Int)
  data MDist USegd s = MDUSegd !(MDist (Vector Int) s)
                               !(MDist (Vector Int) s)
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

  deepSeqD segd z = deepSeqD (lengthsUSegd  segd)
                  $ deepSeqD (indicesUSegd  segd)
                  $ deepSeqD (elementsUSegd segd) z

  sizeD  (DUSegd  _ _ eles) = sizeD eles
  sizeMD (MDUSegd _ _ eles) = sizeMD eles

  measureD segd = "Segd " ++ show (lengthUSegd segd) ++ " " ++ show (elementsUSegd segd)

lengthUSegdD :: Dist USegd -> Dist Int
{-# INLINE_DIST lengthUSegdD #-}
lengthUSegdD (DUSegd lens _ _) = lengthD lens

lengthsUSegdD :: Dist USegd -> Dist (Vector Int)
{-# INLINE_DIST lengthsUSegdD #-}
lengthsUSegdD (DUSegd lens _ _ ) = lens

indicesUSegdD :: Dist USegd -> Dist (Vector Int)
{-# INLINE_DIST indicesUSegdD #-}
indicesUSegdD (DUSegd _ idxs _) = idxs

elementsUSegdD :: Dist USegd -> Dist Int
{-# INLINE_DIST elementsUSegdD #-}
elementsUSegdD (DUSegd _ _ dns) = dns

