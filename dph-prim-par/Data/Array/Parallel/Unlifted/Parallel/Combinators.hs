{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel combinators for unlifted arrays. 
module Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP, filterUP, packUP, combineUP, combine2UP,
  zipWithUP, foldUP, foldlUP, fold1UP, foldl1UP, scanUP
) 
where
import Data.Array.Parallel.Base
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.UPSel
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Parallel.Combinators." Prelude.++ s


-- | Apply a worker to all elements of an array.
mapUP :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE_UP mapUP #-}
mapUP f xs 
        = splitJoinD theGang (mapD theGang (Seq.map f)) xs


-- | Keep elements that match the given predicate.
filterUP :: Unbox a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE_UP filterUP #-}
filterUP f
        = joinD  theGang unbalanced
        . mapD   theGang (Seq.filter f)
        . splitD theGang unbalanced


-- | Take elements of an array where a flag value is true, and pack them into
--   the result. 
-- 
--   * The souce and flag arrays must have the same length, but this is not checked.
--
packUP :: Unbox e => Vector e -> Vector Bool -> Vector e
{-# INLINE_UP packUP #-}
packUP xs flags 
        = Seq.fsts . filterUP snd $ Seq.zip xs flags


-- | Combine two vectors based on a selector. 
--   If the selector is true then take the element from the first vector, 
--   otherwise take it from the second.
--
--   * The data vectors must have enough elements to satisfy the flag vector, 
--     but this is not checked.
--  
combineUP :: Unbox a => Vector Bool -> Vector a -> Vector a -> Vector a
{-# INLINE combineUP #-}
combineUP flags xs ys 
        = combine2UP tags (mkUPSelRep2 tags) xs ys
        where tags = Seq.map (fromBool . not) flags


-- | Combine two vectors based on a selector. 
--
--   * The data vectors must have enough elements to satisfy the selector,
--     but this is not checked.
--
combine2UP :: Unbox a => Vector Tag -> UPSelRep2 -> Vector a -> Vector a -> Vector a
{-# INLINE_UP combine2UP #-}
combine2UP tags rep !xs !ys 
        = joinD    theGang balanced
        $ zipWithD theGang go rep
        $ splitD   theGang balanced tags
        where   go ((i,j), (m,n)) ts 
                 = Seq.combine2ByTag ts 
                        (Seq.slice xs i m)
                        (Seq.slice ys j n)
    

-- | Apply a worker function to correponding elements of two arrays.
zipWithUP :: (Unbox a, Unbox b, Unbox c) 
          => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE_UP zipWithUP #-}
zipWithUP f xs ys
        = splitJoinD theGang 
                (mapD theGang (Seq.map (uncurry f))) 
                (Seq.zip xs ys)


-- | Undirected fold.
--   Note that this function has more constraints on its parameters than the
--   standard fold function from the Haskell Prelude.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker.
--     For example 0 is neutral wrt (+) and 1 is neutral wrt (*).
--
--   We need these constraints so that we can partition the fold across 
--   several threads. Each thread folds a chunk of the input vector, 
--   then we fold together all the results in the main thread.
--
foldUP  :: (Unbox a, DT a) => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE_UP foldUP #-}
foldUP f !z xs
        = foldD theGang f
                (mapD   theGang (Seq.fold f z)
                (splitD theGang unbalanced xs))


-- | Left fold over an array. 
--
--   * If the vector is empty then this returns the provided neural element.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker,
--     see `foldUP` for discussion.
--
foldlUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE_UP foldlUP #-}
foldlUP f z arr 
  | Seq.null arr = z
  | otherwise    = foldl1UP f arr


-- | Alias for `foldl1UP`
fold1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
{-# INLINE_UP fold1UP #-}
fold1UP = foldl1UP


-- | Left fold over an array, using the first element of the vector as the
--   neural element.
--
--   * If the vector contains no elements then you'll get a bounds-check error.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker,
--     see `foldUP` for discussion.
--
foldl1UP :: (DT a, Unbox a) => (a -> a -> a) -> Vector a -> a
{-# INLINE_UP foldl1UP #-}
foldl1UP f arr 
        = (maybe z (f z)
        . foldD  theGang combine'
        . mapD   theGang (Seq.foldl1Maybe f)
        . splitD theGang unbalanced) arr
        where
                z = Seq.index (here "fold1UP") arr 0
                combine' (Just x) (Just y) = Just (f x y)
                combine' (Just x) Nothing  = Just x
                combine' Nothing  (Just y) = Just y
                combine' Nothing  Nothing  = Nothing


-- | Prefix scan. Similar to fold, but produce an array of the intermediate states.
--
--   * The worker function must be associative.
-- 
--   * The provided starting element must be neutral with respect to the worker,
--     see `foldUP` for discussion.
--
scanUP :: (DT a, Unbox a) => (a -> a -> a) -> a -> Vector a -> Vector a
{-# INLINE_UP scanUP #-}
scanUP f z 
        = splitJoinD theGang go
        where   go xs = let (ds,zs) = unzipD $ mapD theGang (Seq.scanRes f z) xs
                            zs'     = fst (scanD theGang f z zs)
                        in  zipWithD theGang (Seq.map . f) zs' ds

