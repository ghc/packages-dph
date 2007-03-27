module Pipelines where

import Data.Array.Parallel.Unlifted

pipe1 :: UArr Int -> UArr Int -> UArr Int
pipe1 xs ys = mapU (+1) (xs +:+ ys)
{-# NOINLINE pipe1 #-}

pipe2 :: UArr Int -> UArr Int
pipe2 = mapU (+1) . tailU
{-# NOINLINE pipe2 #-}

pipe3 :: UArr Int -> Int
pipe3 = maximumU . scan1U (+)
{-# NOINLINE pipe3 #-}

pipe4 :: SUArr Int -> Int
pipe4 = maximumU . sumSU
{-# NOINLINE pipe4 #-}

