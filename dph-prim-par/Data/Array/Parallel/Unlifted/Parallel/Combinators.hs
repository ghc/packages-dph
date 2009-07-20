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
  mapUP, filterUP, packUP, combineUP, zipWithUP, foldUP, fold1UP, foldl1UP,
  scanUP
) where

import Data.Array.Parallel.Base
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed

mapUP :: (UA a, UA b) => (a -> b) -> UArr a -> UArr b
{-# INLINE mapUP #-}
mapUP f = splitJoinD theGang (mapD theGang (mapU f))

filterUP :: UA a => (a -> Bool) -> UArr a -> UArr a
{-# INLINE filterUP #-}
filterUP f = joinD  theGang unbalanced
           . mapD   theGang (filterU f)
           . splitD theGang unbalanced



-- |Extract all elements from an array according to a given flag array
-- 
packUP:: UA e => UArr e -> UArr Bool -> UArr e
{-# INLINE_UP packUP #-}
packUP xs flags = fstU . filterUP sndS $  zipU xs flags

combineUP :: UA a => UArr Bool -> UArr a -> UArr a -> UArr a
{-# INLINE_UP combineUP #-}
combineUP flags !xs !ys = joinD theGang balanced
                        . zipWithD theGang go (zipD is ns)
                        $ splitD theGang balanced flags
  where
    ns = mapD theGang count
       $ splitD theGang balanced flags

    is = fstS $ scanD theGang add (0 :*: 0) ns

    count bs = let ts = sumU (mapU fromBool bs)
               in ts :*: (lengthU bs - ts)

    add (x1 :*: y1) (x2 :*: y2) = (x1 + x2) :*: (y1 + y2)

    go ((i :*: j) :*: (m :*: n)) bs = combineU bs (sliceU xs i m) (sliceU ys j n)


zipWithUP :: (UA a, UA b, UA c) => (a -> b -> c) -> UArr a -> UArr b -> UArr c
{-# INLINE zipWithUP #-}
zipWithUP f xs ys = splitJoinD theGang (mapD theGang (mapU (uncurryS f))) (zipU xs ys)
{-
zipWithUP f a b = joinD    theGang balanced
                 (zipWithD theGang (zipWithU f)
                    (splitD theGang balanced a)
                    (splitD theGang balanced b))
-}
--zipWithUP f a b = mapUP (uncurryS f) (zipU a b)

foldUP :: (UA a, DT a) => (a -> a -> a) -> a -> UArr a -> a
{-# INLINE foldUP #-}
foldUP f z xs = maybeS z (f z)
               (foldD  theGang combine
               (mapD   theGang (foldl1MaybeU f)
               (splitD theGang unbalanced
                xs)))
  where
    combine (JustS x) (JustS y) = JustS (f x y)
    combine (JustS x) NothingS  = JustS x
    combine NothingS  (JustS y) = JustS y
    combine NothingS  NothingS  = NothingS


-- |Array reduction proceeding from the left (requires associative combination)
--
foldlUP :: (DT a, UA a) => (a -> a -> a) -> a -> UArr a -> a
{-# INLINE_UP foldlUP #-}
foldlUP f z arr 
  | nullU arr = z
  | otherwise = foldl1UP f arr

-- |Reduction of a non-empty array which requires an associative combination
-- function
--
fold1UP :: (DT a, UA a) => (a -> a -> a) -> UArr a -> a
{-# INLINE fold1UP #-}
fold1UP = foldl1UP



foldl1UP :: (DT a, UA a) => (a -> a -> a) -> UArr a -> a
{-# INLINE_U foldl1UP #-}
foldl1UP f arr = (maybeS z (f z)
           . foldD  theGang combine
           . mapD   theGang (foldl1MaybeU f)
           . splitD theGang unbalanced) arr
  where
    z = arr !: 0
    combine (JustS x) (JustS y) = JustS (f x y)
    combine (JustS x) NothingS  = JustS x
    combine NothingS  (JustS y) = JustS y
    combine NothingS  NothingS  = NothingS

scanUP :: (DT a, UA a) => (a -> a -> a) -> a -> UArr a -> UArr a
{-# INLINE_UP scanUP #-}
scanUP f z = splitJoinD theGang go
  where
    go xs = let ds :*: zs = unzipD $ mapD theGang (scanResU f z) xs
                zs'       = fstS (scanD theGang f z zs)
            in
            zipWithD theGang (mapU . f) zs' ds


