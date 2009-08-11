{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hierarchical where 


import qualified Array as A
import qualified Data.Array.Parallel.Unlifted as U

import Control.Exception (evaluate, assert)
import System.Console.GetOpt
import qualified System.Random as R

import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U
import Control.Exception (evaluate)

data HMatrix a =  HMatrix { hmThreshold:: Int
                          , hmOrder    :: Int
                          , hmDA       :: A.DArray A.DIM2 a
                          }


-- Just changes the representation, not the ordering of the elements.
--
toHMatrix:: (U.Elt a) => Int -> A.Array A.DIM2 a -> HMatrix a
{-# INLINE toHMatrix #-}
toHMatrix threshold arr =
   HMatrix{ hmThreshold =  threshold
          , hmOrder     =  order
          , hmDA        =  A.toDArray  arr
          }
   where
     (_:*: order) = A.arrayShape arr


 

($$):: (A.DArray A.DIM2 a -> b) -> HMatrix a -> b
($$) f hm = f $ hmDA hm

hmSplit:: U.Elt a => HMatrix a -> (HMatrix a, HMatrix a, HMatrix a, HMatrix a)
{-# INLINE hmSplit #-}
hmSplit hm = assert (hmOrder hm > 1) $
  (t1,t2,t3,t4)
  where
    n2 = (hmOrder hm) `div` 2
    t1 = hm{ hmOrder = n2, hmDA    = (A.da_tile $$ hm) (():*: 0 :*: 0)   (() :*: n2 :*: n2)}
    t2 = hm{ hmOrder = n2, hmDA    = (A.da_tile $$ hm) (():*: n2 :*: 0)  (() :*: n2 :*: n2)}
    t3 = hm{ hmOrder = n2, hmDA    = (A.da_tile $$ hm) (():*: 0 :*: n2)  (() :*: n2 :*: n2)}
    t4 = hm{ hmOrder = n2, hmDA    = (A.da_tile $$ hm) (():*: n2 :*: n2) (() :*: n2 :*: n2)}

hmJoin:: U.Elt a => (HMatrix a, HMatrix a, HMatrix a, HMatrix a) -> HMatrix a 
{-# INLINE hmJoin #-}
hmJoin (t1,t2,t3,t4) = assert (hmOrder t1 > 1) $
    t1{hmDA = A.da_append (A.da_append (hmDA t1) (hmDA t2) (() :*: 2 * order :*: order)) 
                        (A.da_append (hmDA t3) (hmDA t4) (() :*: 2 * order :*: order))
                        (() :*: 2* order :*: 2* order)}
  where
    order = hmOrder t1    



--  matrix multiplication, assumes square matrices of size 2^n * 2^n
--
hmmult:: HMatrix Double -> HMatrix Double -> HMatrix Double
hmmult a b = 
    if (n <= hmThreshold a) 
    then a{hmDA = A.da_mmMult (hmDA a) (hmDA b)}
    else hmJoin (r1,r2,r3,r4)

  where
    n = hmOrder a
    m = hmOrder b

    (a1,a2,a3,a4) = hmSplit a
    (b1,b2,b3,b4) = hmSplit b
  
    r1 = a1{hmDA = (A.da_zipWith (+) $$ (hmmult a1 b1)) $$ (hmmult a2 b3)}
    r2 = a2{hmDA = (A.da_zipWith (+) $$ (hmmult a1 b2)) $$ (hmmult a2 b4)}
    r3 = a3{hmDA = (A.da_zipWith (+) $$ (hmmult a3 b1)) $$ (hmmult a4 b3)}
    r4 = a4{hmDA = (A.da_zipWith (+) $$ (hmmult a3 b2)) $$ (hmmult a4 b4)}

