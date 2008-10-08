{-# OPTIONS -fvectorise #-}

module Data.Array.Parallel.Prelude.Word8 (
  Word8,
  (==), (/=), (<=), (<), (>=), (>), min, max,
  minimumP, maximumP, minIndexP, maxIndexP,
  (+), (-), (*), negate, abs, sumP, productP,
  div, mod, toInt, fromInt
) where

import Data.Array.Parallel.Prelude.Base.Word8
import qualified Data.Array.Parallel.Prelude.Base
import Data.Word (Word8)
import Prelude ()

