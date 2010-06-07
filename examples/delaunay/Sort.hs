{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Sort ( median, medianIndex, collect, sort ) where

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

kthSmallestIndex :: [:D.Double:] -> Int -> Int -> Int
kthSmallestIndex xs k i
  | k >= lengthP lts && k < n - lengthP gts = i + lengthP lts
  | otherwise = kthSmallestIndex ys k' i'
  where
    n     = lengthP xs
    pivot = xs !: (n `div` 2)

    lts   = [:x | x <- xs, x D.< pivot:]
    gts   = [:x | x <- xs, x D.> pivot:]

    (ys, k', i') | k < lengthP lts = (lts, k, i)
                 | otherwise       = (gts, k - (n - lengthP gts), i + (n - lengthP gts))

medianIndex :: [:D.Double:] -> Int
medianIndex xs = kthSmallestIndex xs (lengthP xs `div` 2) 0

collect :: [:(Int,Int):] -> [:(Int,[:Int:]):]
collect ps
  | lengthP ps I.== 0 = [::]
  | otherwise =
  let
    (pivot,_) = ps !: (lengthP ps `I.div` 2)
    ls        = [:(i,j) | (i,j) <- ps, i I.< pivot:]
    gs        = [:(i,j) | (i,j) <- ps, i I.> pivot:]
    js        = [:j | (i,j) <- ps, i I.== pivot:]

    ss        = mapP collect [:ls,gs:]
  in
  (ss!:0) +:+ [:(pivot,js):] +:+ (ss!:1)

sort :: [:Int:] -> [:Int:]
sort xs | lengthP xs I.<= 1 = xs
sort xs = (ss!:0) +:+ [:pivot:] +:+ (ss!:1)
  where
    pivot = xs !: (lengthP xs `I.div` 2)
    ls    = [:x | x <- xs, x < pivot:]
    gs    = [:x | x <- xs, x > pivot:]
    ss    = mapP sort [:ls,gs:]

