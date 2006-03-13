-- the infamous sum square fusion example

module SumSq (sumSq)
where

import PArray

sumSq :: Int -> Int
{-# NOINLINE sumSq #-}
--sumSq = sumP . mapP (\x -> x * x) . enumFromToP 1
sumSq n = sumP (mapP (\x -> x * x) (enumFromToP 1 n))
