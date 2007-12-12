{-# OPTIONS -fvectorise #-}

module Data.Array.Parallel.Prelude.Double (
  P.Double, (+), (-), (*), sumP,
  (==), (/=), (<=), (<), (>=), (>)
) where

import Data.Array.Parallel.Prelude.Base
import Data.Array.Parallel.Prelude.Base.Double

import qualified Prelude as P
import Prelude (Double)

infixl 7 *
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=

(==), (/=), (<), (<=), (>), (>=) :: Double -> Double -> P.Bool
(==) = eq
(/=) = neq
(<) = lt
(<=) = le
(>) = gt
(>=) = ge

(*) :: Double -> Double -> Double
(*) = mult

(+) :: Double -> Double -> Double
(+) = plus

(-) :: Double -> Double -> Double
(-) = minus

