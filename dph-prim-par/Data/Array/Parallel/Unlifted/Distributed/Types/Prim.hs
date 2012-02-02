{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- | Distribution of values of primitive types.
module Data.Array.Parallel.Unlifted.Distributed.Types.Prim 
        ( DPrim (..)
        , DT    (..)
        , Dist  (..))
where
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Data.Array.Parallel.Base
import Data.Array.Parallel.Pretty
import Data.Word
import Control.Monad
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Vector.Unboxed.Mutable                    as MV
import Prelude as P

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
primIndexD :: DPrim a => String -> Dist a -> Int -> a
primIndexD str = (V.index (str P.++ "/primIndexD")) . unDPrim
{-# INLINE primIndexD #-}


-- | Create a new distributed value, having as many members as threads
--   in the given 'Gang'.
primNewMD :: DPrim a => Gang -> ST s (MDist a s)
primNewMD = liftM mkMDPrim . MV.new . gangSize
{-# INLINE primNewMD #-}


-- | Read the member of a distributed value corresponding to the given thread index.
primReadMD :: DPrim a => MDist a s -> Int -> ST s a
primReadMD = MV.read . unMDPrim
{-# INLINE primReadMD #-}


-- | Write the member of a distributed value corresponding to the given thread index.
primWriteMD :: DPrim a => MDist a s -> Int -> a -> ST s ()
primWriteMD = MV.write . unMDPrim
{-# INLINE primWriteMD #-}


-- | Freeze a mutable distributed value to an immutable one.
--   You promise not to update the mutable one any further.
primUnsafeFreezeMD :: DPrim a => MDist a s -> ST s (Dist a)
primUnsafeFreezeMD = liftM mkDPrim . V.unsafeFreeze . unMDPrim
{-# INLINE primUnsafeFreezeMD #-}


-- | Get the size of a distributed value, that is, the number of threads
--   in the gang that it was created for.
primSizeD :: DPrim a => Dist a -> Int
primSizeD = V.length . unDPrim
{-# INLINE primSizeD #-}


-- | Get the size of a distributed mutable value, that is, the number of threads
--   in the gang it was created for.
primSizeMD :: DPrim a => MDist a s -> Int
primSizeMD = MV.length . unMDPrim
{-# INLINE primSizeMD #-}


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


-- Ordering -----------------------------------------------------------------------
instance DPrim Ordering where
  mkDPrim               = DOrdering
  unDPrim (DOrdering a) = a

  mkMDPrim                = MDOrdering
  unMDPrim (MDOrdering a) = a


instance DT Ordering where
  data Dist  Ordering   = DOrdering  !(V.Vector    Ordering)
  data MDist Ordering s = MDOrdering !(MV.STVector s Ordering)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD


-- Integer -----------------------------------------------------------------------
-- FIXME: fake instances
instance DPrim Integer
instance DT Integer


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

  measureD n = "Int " P.++ show n

instance PprPhysical (Dist Int) where
 pprp (DInt xs)
  =  text "DInt" <+> text (show $ V.toList xs)


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


