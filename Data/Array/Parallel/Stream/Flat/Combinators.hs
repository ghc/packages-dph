module Data.Array.Parallel.Stream.Flat.Combinators (
  mapS, filterS, foldS, scanS,
  zipWithS, zipWith3S, zipS
) where

import Data.Array.Parallel.Base (
  (:*:)(..))
import Data.Array.Parallel.Stream.Flat.Stream

-- | Mapping
--
mapS :: (a -> b) -> Stream a -> Stream b
{-# INLINE [1] mapS #-}
mapS f (Stream next s n) = Stream next' s n
  where
    next' = fmap f . next

-- | Filtering
--
filterS :: (a -> Bool) -> Stream a -> Stream a
{-# INLINE [1] filterS #-}
filterS f (Stream next s n) = Stream next' s n
  where
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
                 Done        -> z
                 Skip s' -> fold z s'
                 Yield x s'  -> fold (f z x) s'

-- | Scanning
--
scanS :: (b -> a -> b) -> b -> Stream a -> Stream b
{-# INLINE [1] scanS #-}
scanS f z (Stream next s n) = Stream next' (z :*: s) n
  where
    next' (z :*: s) = case next s of
                        Done -> Done
                        Skip s' -> Skip (z :*: s')
                        Yield x s'  -> Yield z (f z x :*: s')

-- | Zipping
--
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
{-# INLINE [1] zipWithS #-}
zipWithS f (Stream next1 s m) (Stream next2 t n) =
  Stream next (s :*: t) m
  where
    next (s :*: t) =
      case next1 s of
        Done        -> Done
        Skip s' -> Skip (s' :*: t)
        Yield x s'  -> case next2 t of
                         Done -> Done
                         Skip t' -> Skip (s :*: t')
                         Yield y t'  -> Yield (f x y) (s' :*: t')

zipWith3S :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
{-# INLINE [1] zipWith3S #-}
zipWith3S f (Stream next1 s1 n) (Stream next2 s2 _) (Stream next3 s3 _) =
  Stream next (s1 :*: s2 :*: s3) n
  where
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

