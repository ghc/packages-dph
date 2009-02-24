module Pipelines where

import Data.Array.Parallel.Unlifted as U

pipe1 :: U.Array Int -> U.Array Int -> U.Array Int
pipe1 xs ys = U.map (+1) (xs +:+ ys)
{-# NOINLINE pipe1 #-}

pipe2 :: U.Array Int -> U.Array Int
pipe2 = U.map (+1) . U.tail
{-# NOINLINE pipe2 #-}

pipe3 :: U.Array Int -> Int
pipe3 = U.maximum . U.scan1 (+)
{-# NOINLINE pipe3 #-}

pipe4 :: U.SArray Int -> Int
pipe4 = U.maximum . U.sum_s
{-# NOINLINE pipe4 #-}

pipe5 :: U.Array Int -> U.Array Int
{-# NOINLINE pipe5 #-}
pipe5 xs = U.sum_s (U.replicate_s (U.replicate (U.length xs) 5) xs)

