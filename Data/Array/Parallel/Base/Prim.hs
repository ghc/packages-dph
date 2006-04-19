-- | This is a hack to avoid a lot of tedious typing. It will go away once we
-- have associated types.

module Data.Array.Parallel.Base.Prim (
  Prim(..), MPrim(..),
  unPrim, unMPrim, mkPrim, mkMPrim
) where

import Data.Array.Parallel.Base.BUArr  ( BUArr, MBUArr )
import Data.Array.Parallel.Base.Hyperstrict ( HS )

data Prim e where
  PrimBool   :: !(BUArr Bool)   -> Prim Bool
  PrimChar   :: !(BUArr Char)   -> Prim Char
  PrimInt    :: !(BUArr Int)    -> Prim Int
  PrimFloat  :: !(BUArr Float)  -> Prim Float
  PrimDouble :: !(BUArr Double) -> Prim Double

data MPrim e s where
  MPrimBool   :: !(MBUArr s Bool)   -> MPrim Bool s
  MPrimChar   :: !(MBUArr s Char)   -> MPrim Char s
  MPrimInt    :: !(MBUArr s Int)    -> MPrim Int s
  MPrimFloat  :: !(MBUArr s Float)  -> MPrim Float s
  MPrimDouble :: !(MBUArr s Double) -> MPrim Double s

instance HS (Prim e)
instance HS (MPrim e s)

unPrim :: Prim e -> BUArr e
unPrim (PrimBool   arr) = arr
unPrim (PrimChar   arr) = arr
unPrim (PrimInt    arr) = arr
unPrim (PrimFloat  arr) = arr
unPrim (PrimDouble arr) = arr

unMPrim :: MPrim e s -> MBUArr s e
unMPrim (MPrimBool   arr) = arr
unMPrim (MPrimChar   arr) = arr
unMPrim (MPrimInt    arr) = arr
unMPrim (MPrimFloat  arr) = arr
unMPrim (MPrimDouble arr) = arr

class Primitive e where
  mkPrim  :: BUArr  e   -> Prim e
  mkMPrim :: MBUArr s e -> MPrim e s

instance Primitive Bool where
  mkPrim  = PrimBool
  mkMPrim = MPrimBool

instance Primitive Char where
  mkPrim  = PrimChar
  mkMPrim = MPrimChar

instance Primitive Int where
  mkPrim  = PrimInt
  mkMPrim = MPrimInt

instance Primitive Float where
  mkPrim  = PrimFloat
  mkMPrim = MPrimFloat

instance Primitive Double where
  mkPrim  = PrimDouble
  mkMPrim = MPrimDouble

