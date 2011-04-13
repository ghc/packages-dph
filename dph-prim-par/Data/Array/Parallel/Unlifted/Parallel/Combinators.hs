-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Combinators
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Parallel combinators for unlifted arrays
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP, filterUP, packUP, combineUP, combine2UP,
  zipWithUP, foldUP, fold1UP, foldl1UP, scanUP
) where

import Data.Array.Parallel.Base
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.UPSel


mapUP :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE mapUP #-}
mapUP f xs = splitJoinD theGang (mapD theGang (Seq.map f)) xs


filterUP :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filterUP #-}
filterUP f = joinD  theGang unbalanced
           . mapD   theGang (Seq.filter f)
           . splitD theGang unbalanced


-- |Extract all elements from an array according to a given flag array
-- 
packUP:: Unbox e => Vector e -> Vector Bool -> Vector e
{-# INLINE_UP packUP #-}
packUP xs flags = Seq.fsts . filterUP snd $ Seq.zip xs flags

combineUP :: Unbox a => Vector Bool -> Vector a -> Vector a -> Vector a
{-# INLINE combineUP #-}
combineUP flags xs ys = combine2UP tags (mkUPSelRep2 tags) xs ys
  where
    tags = Seq.map fromBool flags


combine2UP :: Unbox a => Vector Tag -> UPSelRep2 -> Vector a -> Vector a -> Vector a
{-# INLINE_UP combine2UP #-}
combine2UP tags rep !xs !ys = joinD theGang balanced
                            $ zipWithD theGang go rep
                            $ splitD theGang balanced tags
  where
    go ((i,j), (m,n)) ts = Seq.combine2ByTag ts (Seq.slice xs i m)
                                             (Seq.slice ys j n)
    

zipWithUP :: (Unbox a, Unbox b, Unbox c) => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWithUP #-}
zipWithUP f xs ys = splitJoinD theGang (mapD theGang (Seq.map (uncurry f))) (Seq.zip xs ys)


foldUP :: (Unbox a, DT a) => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE foldUP #-}
foldUP f !z xs = foldD  theGang f
                (mapD   theGang (Seq.fold f z)
                (splitD theGang unbalanced xs))


-- |Array reduction proceeding from the left (requires associative combination)
--
foldlUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE_UP foldlUP #-}
foldlUP f z arr 
  | Seq.null arr = z
  | otherwise = foldl1UP f arr


-- |Reduction of a non-empty array which requires an associative combination
-- function
--
fold1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
{-# INLINE fold1UP #-}
fold1UP = foldl1UP


foldl1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
{-# INLINE_U foldl1UP #-}
foldl1UP f arr = (maybe z (f z)
           . foldD  theGang combine
           . mapD   theGang (Seq.foldl1Maybe f)
           . splitD theGang unbalanced) arr
  where
    z = arr ! 0
    combine (Just x) (Just y) = Just (f x y)
    combine (Just x) Nothing  = Just x
    combine Nothing  (Just y) = Just y
    combine Nothing  Nothing  = Nothing


scanUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> Vector a
{-# INLINE_UP scanUP #-}
scanUP f z = splitJoinD theGang go
  where
    go xs = let (ds,zs) = unzipD $ mapD theGang (Seq.scanRes f z) xs
                zs'     = fst (scanD theGang f z zs)
            in
            zipWithD theGang (Seq.map . f) zs' ds


