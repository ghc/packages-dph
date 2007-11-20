{-# OPTIONS -fvectorise #-}
module Data.Array.Parallel.Prelude.Bool (
  Bool(..),

  otherwise, (&&), (||), not
) where

import Data.Array.Parallel.Prelude.Base

import qualified Prelude as P
import Prelude (Bool(..))

infixr 3 &&
infixr 2 ||

otherwise :: Bool
otherwise = True

(&&), (||) :: Bool -> Bool -> Bool

False && p = False
True  && p = p

True  || p = True
False || p = p

not :: Bool -> Bool
not True  = False
not False = True

