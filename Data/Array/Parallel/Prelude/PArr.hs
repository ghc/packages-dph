module Data.Array.Parallel.Prelude.PArr (
  PArr,
  mapP, zipWithP
) where

data PArr a

mapP :: (a -> b) -> PArr a -> PArr b
{-# NOINLINE mapP #-}
mapP _ _ = error "PArr.mapP"

zipWithP :: (a -> b -> c) -> PArr a -> PArr b -> PArr c
{-# NOINLINE zipWithP #-}
zipWithP _ _ _ = error "PArr.zipWithP"

