{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Sort ( kthSmallest, median, collect, sort ) where

import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int as I

import qualified Prelude as P

kthSmallest :: [:D.Double:] -> Int -> D.Double
kthSmallest xs k
  | k >= lengthP lts && k < n - lengthP gts = pivot
  | otherwise = kthSmallest ys k'
  where
    n     = lengthP xs
    pivot = xs !: (n `div` 2)

    lts   = [:x | x <- xs, x D.< pivot:]
    gts   = [:x | x <- xs, x D.> pivot:]

    (ys, k') | k < lengthP lts = (lts, k)
             | otherwise       = (gts, k - (n - lengthP gts))

median :: [:D.Double:] -> D.Double
median xs = kthSmallest xs (lengthP xs `div` 2)

union :: [:(Int,[:Int:]):] -> [:(Int,[:Int:]):]
union ps
  | lengthP ps I.<= 1 = ps
  | otherwise =
  let
    (pivot,_) = ps !: (lengthP ps `I.div` 2)
    ls        = [:(p,xs) | (p,xs) <- ps, p I.< pivot:]
    gs        = [:(p,xs) | (p,xs) <- ps, p I.> pivot:]
    eqs       = [:xs     | (p,xs) <- ps, p I.== pivot:]

    ss        = mapP union [:ls,gs:]
  in
  (ss!:0) +:+ [:(pivot, concatP eqs):] +:+ (ss!:1)

collect :: [:(Int,Int):] -> [:(Int,[:Int:]):]
collect ps = union [:(i,[:j:]) | (i,j) <- ps:]

sort :: [:Int:] -> [:Int:]
sort xs | lengthP xs I.<= 1 = xs
sort xs = (ss!:0) +:+ [:pivot:] +:+ (ss!:1)
  where
    pivot = xs !: (lengthP xs `I.div` 2)
    ls    = [:x | x <- xs, x < pivot:]
    gs    = [:x | x <- xs, x > pivot:]
    ss    = mapP sort [:ls,gs:]

