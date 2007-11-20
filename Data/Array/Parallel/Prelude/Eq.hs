{-# OPTIONS -fvectorise #-}
module Data.Array.Parallel.Prelude.Eq (
  Eq(..)
) where

import Data.Array.Parallel.Prelude.Bool

import Data.Array.Parallel.Prelude.Base
import qualified Prelude as P

infix 4 ==, /=

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x == y = not (x /= y)
  x /= y = not (x == y)

{-
instance Eq () where
  () == () = True

instance (Eq a, Eq b) => Eq (a,b) where
  (x1,y1) == (x2,y2) = x1 == x2 && y1 == y2

instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False
-}

