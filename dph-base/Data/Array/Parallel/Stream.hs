-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream
-- Copyright   : (c) 2010        Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Stream functions not implemented in vector
--

#include "fusion-phases.h"

module Data.Array.Parallel.Stream (
  indexedS, replicateEachS, replicateEachRS,
  interleaveS, combine2ByTagS,
  enumFromToEachS, enumFromStepLenEachS,
  foldSS, fold1SS, combineSS, appendSS,
  foldValuesR,
  indicesSS
) where

import Data.Array.Parallel.Base ( Tag )

import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Stream.Size    ( Size(..) )

indexedS :: S.Stream a -> S.Stream (Int,a)
{-# INLINE_STREAM indexedS #-}
indexedS (Stream next s n) = Stream next' (0,s) n
  where
    {-# INLINE_INNER next' #-}
    next' (i,s) = do
                    r <- next s
                    case r of
                      Yield x s' -> return $ Yield (i,x) (i+1,s')
                      Skip    s' -> return $ Skip        (i,s')
                      Done       -> return Done

replicateEachS :: Int -> S.Stream (Int,a) -> S.Stream a
{-# INLINE_STREAM replicateEachS #-}
replicateEachS n (Stream next s _) =
  Stream next' (0,Nothing,s) (Exact n)
  where
    {-# INLINE next' #-}
    next' (0, _, s) =
      do
        r <- next s
        case r of
          Done           -> return Done
          Skip s'        -> return $ Skip (0, Nothing, s')
          Yield (k,x) s' -> return $ Skip (k, Just x,s')
    next' (k,Nothing,s) = return Done   -- FIXME: unreachable
    next' (k,Just x,s)  = return $ Yield x (k-1,Just x,s)

-- | Repeat each element in the stream n times
--
replicateEachRS :: Int -> S.Stream a -> S.Stream a
{-# INLINE_STREAM replicateEachRS #-}
replicateEachRS !n (Stream next s sz)
  = Stream next' (0,Nothing,s) (sz `multSize` n)
  where
    next' (0,_,s) =
      do
        r <- next s
        case r of
          Done       -> return Done
          Skip    s' -> return $ Skip (0,Nothing,s')
          Yield x s' -> return $ Skip (n,Just x,s')
    next' (i,Nothing,s) = return Done -- unreachable
    next' (i,Just x,s) = return $ Yield x (i-1,Just x,s)

multSize :: Size -> Int -> Size
multSize (Exact n) k = Exact (n*k)
multSize (Max   n) k = Max   (n*k)
multSize Unknown   _ = Unknown

-- | Interleave the elements of two streams
--
interleaveS :: S.Stream a -> S.Stream a -> S.Stream a
{-# INLINE_STREAM interleaveS #-}
interleaveS (Stream next1 s1 n1) (Stream next2 s2 n2)
  = Stream next (False,s1,s2) (n1+n2)
  where
    {-# INLINE next #-}
    next (False,s1,s2) =
      do
        r <- next1 s1
        case r of
          Yield x s1' -> return $ Yield x (True ,s1',s2)
          Skip    s1' -> return $ Skip    (False,s1',s2)
          Done        -> return Done

    next (True,s1,s2) =
      do
        r <- next2 s2
        case r of
          Yield x s2' -> return $ Yield x (False,s1,s2')
          Skip    s2' -> return $ Skip    (True ,s1,s2')
          -- FIXME: error
          Done        -> return Done

combine2ByTagS :: S.Stream Tag -> S.Stream a -> S.Stream a -> S.Stream a
{-# INLINE_STREAM combine2ByTagS #-}
combine2ByTagS (Stream next_tag s m) (Stream next0 s0 _)
                                     (Stream next1 s1 _)
  = Stream next (Nothing,s,s0,s1) m
  where
    {-# INLINE_INNER next #-}
    next (Nothing,s,s0,s1)
      = do
          r <- next_tag s
          case r of
            Done       -> return Done
            Skip    s' -> return $ Skip (Nothing,s',s0,s1)
            Yield t s' -> return $ Skip (Just t, s',s0,s1)

    next (Just 0,s,s0,s1)
      = do
          r <- next0 s0
          case r of
            Done        -> error "combine2ByTagS: stream 1 too short"
            Skip    s0' -> return $ Skip    (Just 0, s,s0',s1)
            Yield x s0' -> return $ Yield x (Nothing,s,s0',s1)

    next (Just t,s,s0,s1)
      = do
          r <- next1 s1
          case r of
            Done        -> error "combine2ByTagS: stream 2 too short"
            Skip    s1' -> return $ Skip    (Just t, s,s0,s1')
            Yield x s1' -> return $ Yield x (Nothing,s,s0,s1')

enumFromToEachS :: Int -> S.Stream (Int,Int) -> S.Stream Int
{-# INLINE_STREAM enumFromToEachS #-}
enumFromToEachS n (Stream next s _) = Stream next' (Nothing,s) (Exact n)
  where
    {-# INLINE_INNER next' #-}
    next' (Nothing,s)
      = do
          r <- next s
          case r of
            Yield (k,m) s' -> return $ Skip (Just (k,m),s')
            Skip        s' -> return $ Skip (Nothing,   s')
            Done           -> return Done

    next' (Just (k,m),s)
      | k > m     = return $ Skip    (Nothing,     s)
      | otherwise = return $ Yield k (Just (k+1,m),s)

enumFromStepLenEachS :: Int -> S.Stream (Int,Int,Int) -> S.Stream Int 
{-# INLINE_STREAM enumFromStepLenEachS #-}
enumFromStepLenEachS len (Stream next s _)
  = Stream next' (Nothing,s) (Exact len)
  where
    {-# INLINE_INNER next' #-}
    next' (Nothing,s) 
      = do
          r <- next s
          case r of
            Yield (from,step,len) s' -> return $ Skip (Just (from,step,len),s')
            Skip                  s' -> return $ Skip (Nothing,s')
            Done                     -> return Done

    next' (Just (from,step,0),s) = return $ Skip (Nothing,s)
    next' (Just (from,step,n),s)
      = return $ Yield from (Just (from+step,step,n-1),s)

foldSS :: (a -> b -> a) -> a -> S.Stream Int -> S.Stream b -> S.Stream a
{-# INLINE_STREAM foldSS #-}
foldSS f z (Stream nexts ss sz) (Stream nextv vs _) =
  Stream next (Nothing,z,ss,vs) sz
  where
    {-# INLINE next #-}
    next (Nothing,x,ss,vs) =
      do
        r <- nexts ss
        case r of
          Done        -> return Done
          Skip    ss' -> return $ Skip (Nothing,x, ss', vs)
          Yield n ss' -> return $ Skip (Just n, z, ss', vs)

    next (Just 0,x,ss,vs) =
      return $ Yield x (Nothing,z,ss,vs)
    next (Just n,x,ss,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done
                       -- FIXME
                       -- error
                       --  "Stream.Segmented.foldSS: invalid segment descriptor"
          Skip    vs' -> return $ Skip (Just n,x,ss,vs')
          Yield y vs' -> let r = f x y
                         in r `seq` return (Skip (Just (n-1), r, ss, vs'))

fold1SS :: (a -> a -> a) -> S.Stream Int -> S.Stream a -> S.Stream a
{-# INLINE_STREAM fold1SS #-}
fold1SS f (Stream nexts ss sz) (Stream nextv vs _) =
  Stream next (Nothing,Nothing,ss,vs) sz
  where
    {-# INLINE [0] next #-}
    next (Nothing,Nothing,ss,vs) =
      do
        r <- nexts ss
        case r of
          Done        -> return Done
          Skip    ss' -> return $ Skip (Nothing,Nothing,ss',vs)
          Yield n ss' -> return $ Skip (Just n ,Nothing,ss',vs)

    next (Just !n,Nothing,ss,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done -- FIXME: error
          Skip    vs' -> return $ Skip (Just n,    Nothing,ss,vs')
          Yield x vs' -> return $ Skip (Just (n-1),Just x, ss,vs')

    next (Just 0,Just x,ss,vs) =
      return $ Yield x (Nothing,Nothing,ss,vs)

    next (Just n,Just x,ss,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done  -- FIXME: error
          Skip    vs' -> return $ Skip (Just n    ,Just x      ,ss,vs')
          Yield y vs' -> let r = f x y
                         in r `seq` return (Skip (Just (n-1),Just r,ss,vs'))


combineSS:: S.Stream Bool -> S.Stream Int -> S.Stream a
                          -> S.Stream Int -> S.Stream a -> S.Stream a
{-# INLINE_STREAM combineSS #-}
combineSS (Stream nextf sf _) 
          (Stream nexts1 ss1 _) (Stream nextv1 vs1 nv1)
          (Stream nexts2 ss2 _) (Stream nextv2 vs2 nv2)
  = Stream next (Nothing,True,sf,ss1,vs1,ss2,vs2)
                (nv1+nv2)
  where
    {-# INLINE next #-}
    next (Nothing,f,sf,ss1,vs1,ss2,vs2) =
      do
        r <- nextf sf
        case r of
          Done        -> return Done
          Skip sf'    -> return $ Skip (Nothing,f,sf',ss1,vs1,ss2,vs2) 
          Yield c sf'
            | c ->
              do
                r <- nexts1 ss1
                case r of
                  Done         -> return Done
                  Skip ss1'    -> return $ Skip (Nothing,f,sf,ss1',vs1,ss2,vs2) 
                  Yield n ss1' -> return $ Skip (Just n,c,sf',ss1',vs1,ss2,vs2) 

            | otherwise ->
              do
                r <- nexts2 ss2
                case r of
                  Done         -> return Done
                  Skip ss2'    -> return $ Skip (Nothing,f,sf,ss1,vs1,ss2',vs2) 
                  Yield n ss2' -> return $ Skip (Just n,c,sf',ss1,vs1,ss2',vs2)

    next (Just 0,_,sf,ss1,vs1,ss2,vs2) =
         return $ Skip (Nothing,True,sf,ss1,vs1,ss2,vs2)

    next (Just n,True,sf,ss1,vs1,ss2,vs2) =
      do
        r <- nextv1 vs1
        case r of
          Done         -> return Done
          Skip vs1'    -> return $ Skip (Just n,True,sf,ss1,vs1',ss2,vs2) 
          Yield x vs1' -> return $ Yield x (Just (n-1),True,sf,ss1,vs1',ss2,vs2)

    next (Just n,False,sf,ss1,vs1,ss2,vs2) =
      do
        r <- nextv2 vs2
        case r of
          Done         -> return Done
          Skip vs2'    -> return $ Skip (Just n,False,sf,ss1,vs1,ss2,vs2') 
          Yield x vs2' -> return $ Yield x (Just (n-1),False,sf,ss1,vs1,ss2,vs2')


appendSS :: S.Stream Int -> S.Stream a -> S.Stream Int -> S.Stream a -> S.Stream a
{-# INLINE_STREAM appendSS #-}
appendSS (Stream nexts1 ss1 ns1) (Stream nextv1 sv1 nv1)
         (Stream nexts2 ss2 ns2) (Stream nextv2 sv2 nv2)
  = Stream next (True,Nothing,ss1,sv1,ss2,sv2) (nv1 + nv2)
  where
    {-# INLINE next #-}
    next (True,Nothing,ss1,sv1,ss2,sv2) =
      do
        r <- nexts1 ss1
        case r of
          Done         -> return $ Done
          Skip    ss1' -> return $ Skip (True,Nothing,ss1',sv1,ss2,sv2)
          Yield n ss1' -> return $ Skip (True,Just n ,ss1',sv1,ss2,sv2)

    next (True,Just 0,ss1,sv1,ss2,sv2)
      = return $ Skip (False,Nothing,ss1,sv1,ss2,sv2)

    next (True,Just n,ss1,sv1,ss2,sv2) =
      do
        r <- nextv1 sv1
        case r of
          Done         -> return Done  -- FIXME: error
          Skip    sv1' -> return $ Skip (True,Just n,ss1,sv1',ss2,sv2)
          Yield x sv1' -> return $ Yield x (True,Just (n-1),ss1,sv1',ss2,sv2)

    next (False,Nothing,ss1,sv1,ss2,sv2) =
      do
        r <- nexts2 ss2
        case r of
          Done         -> return Done  -- FIXME: error
          Skip    ss2' -> return $ Skip (False,Nothing,ss1,sv1,ss2',sv2)
          Yield n ss2' -> return $ Skip (False,Just n,ss1,sv1,ss2',sv2)

    next (False,Just 0,ss1,sv1,ss2,sv2)
      = return $ Skip (True,Nothing,ss1,sv1,ss2,sv2)

    next (False,Just n,ss1,sv1,ss2,sv2) =
      do
        r <- nextv2 sv2
        case r of
          Done         -> return Done  -- FIXME: error
          Skip    sv2' -> return $ Skip (False,Just n,ss1,sv1,ss2,sv2')
          Yield x sv2' -> return $ Yield x (False,Just (n-1),ss1,sv1,ss2,sv2')

foldValuesR :: (a -> b -> a) -> a -> Int -> S.Stream b -> S.Stream a
{-# INLINE_STREAM foldValuesR #-}
foldValuesR f z segSize (Stream nextv vs nv) =
  Stream next (segSize,z,vs) (nv `divSize` segSize)
  where
    {-# INLINE next #-}  
    next (0,x,vs) = return $ Yield x (segSize,z,vs)

    next (n,x,vs) =
      do
        r <- nextv vs
        case r of
          Done        -> return Done
          Skip    vs' -> return $ Skip (n,x,vs')
          Yield y vs' -> let r = f x y
                         in r `seq` return (Skip ((n-1),r,vs'))

divSize :: Size -> Int -> Size
divSize (Exact n) k = Exact (n `div` k)
divSize (Max   n) k = Max   (n `div` k)
divSize Unknown   _ = Unknown

indicesSS :: Int -> Int -> S.Stream Int -> S.Stream Int
{-# INLINE_STREAM indicesSS #-}
indicesSS n i (Stream next s _) =
  Stream next' (i,Nothing,s) (Exact n)
  where
    {-# INLINE next' #-}
    next' (i,Nothing,s) =
      do
        r <- next s
        case r of
          Done       -> return Done
          Skip    s' -> return $ Skip (i,Nothing,s')
          Yield k s' -> return $ Skip (i,Just k,s')

    next' (i,Just k,s)
      | k > 0     = return $ Yield i (i+1,Just (k-1),s)
      | otherwise = return $ Skip    (0  ,Nothing   ,s)

