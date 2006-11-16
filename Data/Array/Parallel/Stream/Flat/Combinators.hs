-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Combinators
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Higher-order combinators for streams
--

module Data.Array.Parallel.Stream.Flat.Combinators (
  mapS, filterS, foldS, fold1MaybeS, scanS, mapAccumS,
  zipWithS, zipWith3S, zipS
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), Rebox(..), Box(..))
import Data.Array.Parallel.Stream.Flat.Stream

-- | Mapping
--
mapS :: (a -> b) -> Stream a -> Stream b
{-# INLINE [1] mapS #-}
mapS f (Stream next s n) = Stream next' s n
  where
    {-# INLINE next' #-}
    next' s = case next s of
                Done       -> Done
                Skip    s' -> Skip s'
                Yield x s' -> Yield (f x) s'

-- | Filtering
--
filterS :: (a -> Bool) -> Stream a -> Stream a
{-# INLINE [1] filterS #-}
filterS f (Stream next s n) = Stream next' s n
  where
    {-# INLINE next' #-}
    next' s = case next s of
                Done                    -> Done
                Skip s'             -> Skip s'
                Yield x  s' | f x       -> Yield x s'
                            | otherwise -> Skip s'

-- | Folding
-- 
foldS :: (b -> a -> b) -> b -> Stream a -> b
{-# INLINE [1] foldS #-}
foldS f z (Stream next s _) = fold z s
  where
    fold z s = case next s of
                 Done       -> z
                 Skip    s' -> fold z (rebox s')
                 Yield x s' -> fold (f z x) (rebox s')

fold1MaybeS :: (a -> a -> a) -> Stream a -> MaybeS a
{-# INLINE [1] fold1MaybeS #-}
fold1MaybeS f (Stream next s _) = fold0 s
  where
    fold0 s   = case next s of
                  Done       -> NothingS
                  Skip    s' -> fold0 (rebox s')
                  Yield x s' -> fold1 x (rebox s')
    fold1 z s = case next s of
                  Done       -> JustS z
                  Skip    s' -> fold1 z (rebox s')
                  Yield x s' -> fold1 (f z x) (rebox s')

-- | Scanning
--
scanS :: (b -> a -> b) -> b -> Stream a -> Stream b
{-# INLINE [1] scanS #-}
scanS f z (Stream next s n) = Stream next' (Box z :*: s) n
  where
    {-# INLINE next' #-}
    next' (Box z :*: s) = case next s of
                        Done -> Done
                        Skip s' -> Skip (Box z :*: s')
                        Yield x s'  -> Yield z (Box (f z x) :*: s')

mapAccumS :: (acc -> a -> acc :*: b) -> acc -> Stream a -> Stream b
{-# INLINE [1] mapAccumS #-}
mapAccumS f acc (Stream step s n) = Stream step' (s :*: Box acc) n
  where
    step' (s :*: Box acc) = case step s of
                          Done -> Done
                          Skip s' -> Skip (s' :*: Box acc)
                          Yield x s' -> let acc' :*: y = f acc x
                                        in
                                        Yield y (s' :*: Box acc')

-- | Zipping
--
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
{-# INLINE [1] zipWithS #-}
zipWithS f (Stream next1 s m) (Stream next2 t n) =
  Stream next (NothingS :*: s :*: t) m
  where
    {-# INLINE next #-}
    next (NothingS :*: s :*: t) =
      t `seq`
      case next1 s of
        Done       -> Done
        Skip    s' -> Skip (NothingS :*: s' :*: t)
        Yield x s' -> -- Skip (JustS x  :*: s' :*: t)
                      case next2 t of
                        Done       -> Done
                        Skip    t' -> Skip (JustS (Box x) :*: s' :*: t')
                        Yield y t' -> Yield (f x y) (NothingS :*: s' :*: t')
    next (JustS (Box x) :*: s :*: t) =
      s `seq`
      case next2 t of
        Done       -> Done
        Skip t'    -> Skip (JustS (Box x) :*: s :*: t')
        Yield y t' -> Yield (f x y) (NothingS :*: s :*: t')

zipWith3S :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
{-# INLINE [1] zipWith3S #-}
zipWith3S f (Stream next1 s1 n) (Stream next2 s2 _) (Stream next3 s3 _) =
  Stream next (s1 :*: s2 :*: s3) n
  where
    {-# INLINE next #-}
    next (s1 :*: s2 :*: s3) =
      case next1 s1 of
        Done         -> Done
        Skip s1' -> Skip (s1' :*: s2 :*: s3)
        Yield x s1'  ->
          case next2 s2 of
            Done         -> Done
            Skip s2' -> Skip (s1 :*: s2' :*: s3)
            Yield y s2'  ->
              case next3 s3 of
                Done         -> Done
                Skip s3' -> Skip (s1 :*: s2 :*: s3')
                Yield z s3'  -> Yield (f x y z) (s1' :*: s2' :*: s3')

zipS :: Stream a -> Stream b -> Stream (a :*: b)
{-# INLINE zipS #-}
zipS = zipWithS (:*:)

