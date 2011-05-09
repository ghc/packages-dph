{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}

#include "fusion-phases.h"

-- | Instances for the PRepr class.
--
--   For primitive types these are all trivial, as we represent an array of
--   Ints just as an array of Ints. 
--
--   For algebraic data types defined in the source program, the vectoriser
--   creates the appropriate PRepr instances for those types.
--
--   Note that polymorphic container types like tuples and arrays use the 
--   `Wrap` constructor so we only need to convert one layer of the structure
--   to the generic representation at a time. 
--   See "Data.Array.Parallel.PArray.Types" for details.
--
module Data.Array.Parallel.PArray.PReprInstances where
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PDataInstances
import Data.Array.Parallel.PArray.Base
import Data.Array.Parallel.PArray.ScalarInstances
import Data.Array.Parallel.Lifted.Repr
import qualified Data.Array.Parallel.Unlifted as U
import GHC.Word    ( Word8 )


-- Void -----------------------------------------------------------------------
type instance PRepr Void = Void

instance PA Void where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Unit -----------------------------------------------------------------------
type instance PRepr () = ()

instance PA () where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int = Int

instance PA Int where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id



-- Word8 ----------------------------------------------------------------------
type instance PRepr Word8 = Word8

instance PA Word8 where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Float ----------------------------------------------------------------------
type instance PRepr Float = Float

instance PA Float where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double = Double

instance PA Double where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Bool -----------------------------------------------------------------------
data instance PData Bool
  = PBool U.Sel2

type instance PRepr Bool = Sum2 Void Void

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False         = Alt2_1 void
  toPRepr True          = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _)  = False
  fromPRepr (Alt2_2 _)  = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _) = PBool sel


-- Tuple2 ---------------------------------------------------------------------
type instance PRepr (a,b)
        = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a,b) where
  toPRepr (a, b)
        = (Wrap a, Wrap b)

  fromPRepr (Wrap a, Wrap b)
        = (a, b)

  toArrPRepr   (P_2 as bs)
        = P_2 (PWrap as) (PWrap bs)

  fromArrPRepr (P_2 (PWrap as) (PWrap bs))
        = P_2 as bs


-- Tuple3 ---------------------------------------------------------------------
type instance PRepr (a,b,c) 
        = (Wrap a, Wrap b, Wrap c)

instance (PA a, PA b, PA c) => PA (a,b,c) where
  toPRepr (a, b, c)                       
        = (Wrap a, Wrap b, Wrap c)

  fromPRepr (Wrap a, Wrap b, Wrap c)
        = (a, b, c)

  toArrPRepr (P_3 as bs cs)
        = P_3 (PWrap as) (PWrap bs) (PWrap cs)

  fromArrPRepr (P_3 (PWrap as) (PWrap bs) (PWrap cs))
        = P_3 as bs cs


-- Tuple4 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d)
        = (Wrap a, Wrap b, Wrap c, Wrap d)

instance (PA a, PA b, PA c, PA d) => PA (a,b,c,d) where
  toPRepr (a, b, c, d)
        = (Wrap a, Wrap b, Wrap c, Wrap d)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d)
        = (a, b, c, d)

  toArrPRepr (P_4 as bs cs ds)
        = P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds)

  fromArrPRepr (P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds))
        = P_4 as bs cs ds


-- Tuple5 ---------------------------------------------------------------------
type instance PRepr (a,b,c,d,e)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

instance (PA a, PA b, PA c, PA d, PA e) => PA (a,b,c,d,e) where
  toPRepr (a, b, c, d, e)
        = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)
        = (a, b, c, d, e)

  toArrPRepr (P_5 as bs cs ds es)
        = P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) 

  fromArrPRepr (P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es))
        = P_5 as bs cs ds es


-- PArray ---------------------------------------------------------------------
type instance PRepr (PArray a)
        = PArray (PRepr a)

instance PA a => PA (PArray a) where
  {-# INLINE toPRepr #-}
  toPRepr (PArray n# xs) 
        = PArray n# (toArrPRepr xs)

  {-# INLINE fromPRepr #-}
  fromPRepr (PArray n# xs)
        = PArray n# (fromArrPRepr xs)

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PNested segd xs)
        = PNested segd (toArrPRepr xs)

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PNested segd xs)
        = PNested segd (fromArrPRepr xs)

