{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hierarchical where 

import qualified LArray as LA
import qualified Array as A

import Control.Exception (evaluate, assert)
import System.Console.GetOpt
import qualified System.Random as R

import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U
import Control.Exception (evaluate)

-- type HMatrix = (Int, LA.LArray)

--  matrix multiplication, assumes square matrices of size 2^n * 2^n
--
hmmult:: LA.LArray A.DIM2 Double -> LA.LArray A.DIM2 Double -> LA.LArray A.DIM2 Double
hmmult a@(LA.LArray (() :*: n :*: n') _) b@(LA.LArray (() :*: m :*: m') _) = 
  assert (n == m) $ 
         if (n <= 256) 
    then LA.mmMult a b
    else LA.append (LA.append r1 r2 (() :*: 2*q :*: n)) (LA.append r3 r4 (() :*: 2*q :*: n))
                   (() :*: n :*: n)
  where
    q = n `div` 4
    a1 = LA.tile a (() :*: 0   :*: 0)     (() :*: q :*: n)
    a2 = LA.tile a (() :*: q   :*: 0)     (() :*: q :*: n)
    a3 = LA.tile a (() :*: 2*q :*: 0)     (() :*: q :*: n)
    a4 = LA.tile a (() :*: 3*q :*: 0)   (() :*: q :*: n)

    b1 = LA.tile b (() :*: 0   :*: 0)     (() :*: q :*: n)
    b2 = LA.tile b (() :*: q   :*: 0)     (() :*: q :*: n)
    b3 = LA.tile b (() :*: 2*q :*: 0)     (() :*: q :*: n)
    b4 = LA.tile b (() :*: 3*q :*: 0)   (() :*: q :*: n)
  
    r1 = LA.zipWith (+) (hmmult a1 b1) (hmmult a2 b3)
    r2 = LA.zipWith (+) (hmmult a1 b2) (hmmult a2 b4)
    r3 = LA.zipWith (+) (hmmult a3 b1) (hmmult a4 b3)
    r4 = LA.zipWith (+) (hmmult a3 b2) (hmmult a4 b4)


{-
hmmult':: LA.LArray A.DIM2 Int -> LA.LArray A.DIM2 Int -> LA.LArray A.DIM2 Int
hmmult' a@(LA.LArray (() :*: n) _) b@(LA.LArray (() :*: m) _) = 
  assert (n == m) $ 
  if (n <= 128) 
    then LA.mmMult a b
    else LA.append (LA.append r1 r2 (() :*: mid :*: n)) (LA.append r3 r4 (() :*: mid :*: n))
                   (() :*: n :*: n)
  where
    mid = n `div` 2
    a1 = LA.tile a (() :*: 0 :*: 0)     (() :*: mid :*: mid)
    a2 = LA.tile a (() :*: 0 :*: mid)   (() :*: mid :*: mid)
    a3 = LA.tile a (() :*: mid :*: 0)   (() :*: mid :*: mid)
    a4 = LA.tile a (() :*: mid :*: mid) (() :*: mid :*: mid)

    b1 = LA.tile b (() :*: 0 :*: 0)     (() :*: mid :*: mid)
    b2 = LA.tile b (() :*: 0 :*: mid)   (() :*: mid :*: mid)
    b3 = LA.tile b (() :*: mid :*: 0)   (() :*: mid :*: mid)
    b4 = LA.tile b (() :*: mid :*: mid) (() :*: mid :*: mid)
  
    r1 = LA.zipWith (+) (hmmult' a1 b1) (hmmult' a2 b3)
    r2 = LA.zipWith (+) (hmmult' a1 b2) (hmmult' a2 b4)
    r3 = LA.zipWith (+) (hmmult' a3 b1) (hmmult' a4 b3)
    r4 = LA.zipWith (+) (hmmult' a3 b2) (hmmult' a4 b4)-}