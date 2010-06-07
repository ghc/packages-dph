{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Instances (
  PData(..),
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )

import qualified Data.Array.Parallel.Unlifted as U

import GHC.Word    ( Word8 )

---------------------
-- Primitive types --

type instance PRepr Int = Int

instance PA Int where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

type instance PRepr Word8 = Word8

instance PA Word8 where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

type instance PRepr Double = Double

instance PA Double where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

----------
-- Void --

type instance PRepr Void = Void

instance PA Void where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

----------
-- Bool --

data instance PData Bool
  = PBool U.Sel2

type instance PRepr Bool = Sum2 Void Void

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False = Alt2_1 void
  toPRepr True  = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _) = False
  fromPRepr (Alt2_2 _) = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _) = PBool sel

----------
-- Unit --

type instance PRepr () = ()

instance PA () where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

------------
-- Tuples --

type instance PRepr (a,b) = (Wrap a, Wrap b)

instance (PA a, PA b) => PA (a,b) where
  toPRepr (a,b) = (Wrap a, Wrap b)
  fromPRepr (Wrap a, Wrap b) = (a,b)
  toArrPRepr (P_2 as bs) = P_2 (PWrap as) (PWrap bs)
  fromArrPRepr (P_2 (PWrap as) (PWrap bs)) = P_2 as bs

type instance PRepr (a,b,c) = (Wrap a, Wrap b, Wrap c)

instance (PA a, PA b, PA c) => PA (a,b,c) where
  toPRepr (a,b,c) = (Wrap a, Wrap b, Wrap c)
  fromPRepr (Wrap a, Wrap b, Wrap c) = (a,b,c)
  toArrPRepr (P_3 as bs cs) = P_3 (PWrap as) (PWrap bs) (PWrap cs)
  fromArrPRepr (P_3 (PWrap as) (PWrap bs) (PWrap cs)) = P_3 as bs cs

type instance PRepr (a,b,c,d) = (Wrap a, Wrap b, Wrap c, Wrap d)

instance (PA a, PA b, PA c, PA d) => PA (a,b,c,d) where
  toPRepr (a,b,c,d) = (Wrap a, Wrap b, Wrap c, Wrap d)
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d) = (a,b,c,d)
  toArrPRepr (P_4 as bs cs ds)
    = P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds)
  fromArrPRepr (P_4 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds))
    = P_4 as bs cs ds

type instance PRepr (a,b,c,d,e) = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)

instance (PA a, PA b, PA c, PA d, PA e) => PA (a,b,c,d,e) where
  toPRepr (a,b,c,d,e) = (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e)
  fromPRepr (Wrap a, Wrap b, Wrap c, Wrap d, Wrap e) = (a,b,c,d,e)
  toArrPRepr (P_5 as bs cs ds es)
    = P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es) 
  fromArrPRepr (P_5 (PWrap as) (PWrap bs) (PWrap cs) (PWrap ds) (PWrap es))
    = P_5 as bs cs ds es

------------
-- Arrays --

type instance PRepr (PArray a) = PArray (PRepr a)

instance PA a => PA (PArray a) where
  {-# INLINE toPRepr #-}
  toPRepr (PArray n# xs) = PArray n# (toArrPRepr xs)

  {-# INLINE fromPRepr #-}
  fromPRepr (PArray n# xs) = PArray n# (fromArrPRepr xs)

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PNested segd xs) = PNested segd (toArrPRepr xs)

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PNested segd xs) = PNested segd (fromArrPRepr xs)

