-----------------------------------------------------------------------------
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Combinators
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
--
-- | Parallel combinators for unlifted arrays
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


-- | Apply a worker to all elements of a vector.
mapUP :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE mapUP #-}
mapUP f xs 
	= splitJoinD theGang (mapD theGang (Seq.map f)) xs


-- | Keep elements that match the given predicate.
filterUP :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filterUP #-}
filterUP f
	= joinD  theGang unbalanced
        . mapD   theGang (Seq.filter f)
        . splitD theGang unbalanced


-- | Extract all elements from an array according to a given flag array.
--   The two vectors should have the same length, but this is not checked.
packUP :: Unbox e => Vector e -> Vector Bool -> Vector e
{-# INLINE_UP packUP #-}
packUP xs flags 
	= Seq.fsts . filterUP snd $ Seq.zip xs flags


-- | Combine two vectors based on a selector. 
--   If the selector is true then take the element from the first vector, 
--   otherwise take it from the second.
combineUP :: Unbox a => Vector Bool -> Vector a -> Vector a -> Vector a
{-# INLINE combineUP #-}
combineUP flags xs ys 
 	= combine2UP tags (mkUPSelRep2 tags) xs ys
 	where tags = Seq.map fromBool flags


-- | Combine two vectors based on a selector. 
--
--   TODO: What is the differenve between the Tag and the UPSelRep2?
combine2UP :: Unbox a => Vector Tag -> UPSelRep2 -> Vector a -> Vector a -> Vector a
{-# INLINE_UP combine2UP #-}
combine2UP tags rep !xs !ys 
	= joinD    theGang balanced
	$ zipWithD theGang go rep
	$ splitD   theGang balanced tags
	where	go ((i,j), (m,n)) ts 
	 	 = Seq.combine2ByTag ts 
			(Seq.slice xs i m)
			(Seq.slice ys j n)
    

-- | Combine two vectors into a third.
zipWithUP :: (Unbox a, Unbox b, Unbox c) 
          => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWithUP #-}
zipWithUP f xs ys
	= splitJoinD theGang 
		(mapD theGang (Seq.map (uncurry f))) 
		(Seq.zip xs ys)


-- | Undirected fold.
foldUP :: (Unbox a, DT a) => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE foldUP #-}
foldUP f !z xs
	= foldD theGang f
		(mapD   theGang (Seq.fold f z)
		(splitD theGang unbalanced xs))


-- | Array reduction proceeding from the left (requires associative combination)
foldlUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE_UP foldlUP #-}
foldlUP f z arr 
  | Seq.null arr = z
  | otherwise = foldl1UP f arr


-- | Reduction of a non-empty array which requires an associative combination function.
--
--   TODO: What is the difference between this and foldUP above?
--         The two type class constraints are in a different order. Does that matter?
fold1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
{-# INLINE fold1UP #-}
fold1UP = foldl1UP


-- | Same as 'fold1UP'.
foldl1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
{-# INLINE_U foldl1UP #-}
foldl1UP f arr 
	= (maybe z (f z)
	. foldD  theGang combine
	. mapD   theGang (Seq.foldl1Maybe f)
	. splitD theGang unbalanced) arr
	where
		z = arr ! 0
    		combine (Just x) (Just y) = Just (f x y)
    		combine (Just x) Nothing  = Just x
    		combine Nothing  (Just y) = Just y
    		combine Nothing  Nothing  = Nothing


-- | Prefix scan. Similar to fold, but produce an array of the intermediate states.
scanUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> Vector a
{-# INLINE_UP scanUP #-}
scanUP f z 
	= splitJoinD theGang go
	where	go xs = let (ds,zs) = unzipD $ mapD theGang (Seq.scanRes f z) xs
			    zs'     = fst (scanD theGang f z zs)
			in  zipWithD theGang (Seq.map . f) zs' ds
