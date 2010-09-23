{-# LANGUAGE EmptyDataDecls, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed.Arrays
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Operations on distributed arrays.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Distributed.Arrays (
  lengthD, splitLenD, splitLenIdxD,
  splitAsD, splitD, joinLengthD, joinD, splitJoinD, joinDM,
  splitSegdD, splitSegdD', splitSD,

  permuteD, bpermuteD, atomicUpdateD,

  Distribution, balanced, unbalanced
) where

import Data.Array.Parallel.Base ( ST, runST)
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize, seqGang)
import Data.Array.Parallel.Unlifted.Distributed.DistST (
  DistST, stToDistST, myIndex )
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, mkDPrim, indexD, lengthD, newD, writeMD, zipD, unzipD, fstD, sndD,
  elementsUSegdD,
  checkGangD)
import Data.Array.Parallel.Unlifted.Distributed.Basics
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Scalars (
  sumD)

import Data.Bits ( shiftR )
import Control.Monad ( when )

import GHC.Base ( quotInt, remInt )

here s = "Data.Array.Parallel.Unlifted.Distributed.Arrays." Prelude.++ s

data Distribution

balanced :: Distribution
{-# NOINLINE balanced #-}
balanced = error $ here "balanced: touched"

unbalanced :: Distribution
{-# NOINLINE unbalanced #-}
unbalanced = error $ here "unbalanced: touched"

-- | Distribute the given array length over a 'Gang'.
splitLenD :: Gang -> Int -> Dist Int
{-# INLINE splitLenD #-}
splitLenD g n = generateD_cheap g len
  where
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    {-# INLINE [0] len #-}
    len i | i < m     = l+1
          | otherwise = l

splitLenIdxD :: Gang -> Int -> Dist (Int,Int)
{-# INLINE splitLenIdxD #-}
splitLenIdxD g n = generateD_cheap g len_idx
  where
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    {-# INLINE [0] len_idx #-}
    len_idx i | i < m     = (l+1, i*(l+1))
              | otherwise = (l,   i*l + m)
                                               

-- | Distribute an array over a 'Gang' such that each threads gets the given
-- number of elements.
splitAsD :: Unbox a => Gang -> Dist Int -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitAsD #-}
splitAsD g dlen !arr = zipWithD (seqGang g) (Seq.slice arr) is dlen
  where
    is = fst $ scanD g (+) 0 dlen

-- lengthD reexported from types

-- | Overall length of a distributed array.
joinLengthD :: Unbox a => Gang -> Dist (Vector a) -> Int
{-# INLINE joinLengthD #-}
joinLengthD g = sumD g . lengthD


-- NOTE: We need splitD_impl and joinD_impl to avoid introducing loops through
-- rules. Without them, splitJoinD would be a loop breaker.

-- | Distribute an array over a 'Gang'.
splitD_impl :: Unbox a => Gang -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitD_impl #-}
splitD_impl g !arr = generateD_cheap g (\i -> Seq.slice arr (idx i) (len i))
  where
    n = Seq.length arr
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    idx i | i < m     = (l+1)*i
          | otherwise = l*i + m

    len i | i < m     = l+1
          | otherwise = l

    -- slice i | i < m     = Seq.slice arr ((l+1)*i) (l+1)
    --         | otherwise = Seq.slice arr ((l+1)*m + l*(i-m)) l
{-
splitD_impl g !arr = zipWithD (seqGang g) (Seq.slice arr) is dlen
  where
    dlen = splitLengthD (seqGang g) arr
    is   = fstS $ scanD (seqGang g) (+) 0 dlen
-}

-- | Distribute an array over a 'Gang'.
splitD :: Unbox a => Gang -> Distribution -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitD #-}
splitD g _ arr = splitD_impl g arr

joinD_impl :: forall a. Unbox a => Gang -> Dist (Vector a) -> Vector a
{-# INLINE_DIST joinD_impl #-}
joinD_impl g !darr = checkGangD (here "joinD") g darr $
                     Seq.new n (\ma -> zipWithDST_ g (copy ma) di darr)
  where
    (!di,!n) = scanD g (+) 0 $ lengthD darr
    copy :: forall s. MVector s a -> Int -> Vector a -> DistST s ()
    copy ma i arr = stToDistST (Seq.copy (mdrop i ma) arr)

-- | Join a distributed array.
joinD :: Unbox a => Gang -> Distribution -> Dist (Vector a) -> Vector a
{-# INLINE CONLIKE [1] joinD #-}
joinD g _ darr  = joinD_impl g darr

splitJoinD :: (Unbox a, Unbox b)
           => Gang -> (Dist (Vector a) -> Dist (Vector b)) -> Vector a -> Vector b
{-# INLINE_DIST splitJoinD #-}
splitJoinD g f !xs = joinD_impl g (f (splitD_impl g xs))

-- | Join a distributed array, yielding a mutable global array
joinDM :: Unbox a => Gang -> Dist (Vector a) -> ST s (MVector s a)
{-# INLINE joinDM #-}
joinDM g darr = checkGangD (here "joinDM") g darr $
                do
                  marr <- Seq.newM n
                  zipWithDST_ g (copy marr) di darr
                  return marr
  where
    (!di,!n) = scanD g (+) 0 $ lengthD darr
    --
    copy ma i arr = stToDistST (Seq.copy (mdrop i ma) arr)

{-# RULES

"splitD[unbalanced]/joinD" forall g b da.
  splitD g unbalanced (joinD g b da) = da

"splitD[balanced]/joinD" forall g da.
  splitD g balanced (joinD g balanced da) = da

"splitD/splitJoinD" forall g b f xs.
  splitD g b (splitJoinD g f xs) = f (splitD g b xs)

"splitJoinD/joinD" forall g b f da.
  splitJoinD g f (joinD g b da) = joinD g b (f da)

"splitJoinD/splitJoinD" forall g f1 f2 xs.
  splitJoinD g f1 (splitJoinD g f2 xs) = splitJoinD g (f1 . f2) xs

{-
"splitD/Seq.zip" forall g b xs ys.
  splitD g b (Seq.zip xs ys) = zipWithD g Seq.zip (splitD g balanced xs)
                                            (splitD g balanced ys)

"splitJoinD/Seq.zip" forall g f xs ys.
  splitJoinD g f (Seq.zip xs ys)
    = joinD g balanced
        (f (zipWithD g Seq.zip (splitD g balanced xs)
                            (splitD g balanced ys)))

"splitAsD/Seq.zip" forall g dlen xs ys.
  splitAsD g dlen (Seq.zip xs ys) = zipWithD g Seq.zip (splitAsD g dlen xs)
                                                 (splitAsD g dlen ys)
-}
  #-}

{- FIXME

"Seq.fsts/joinD" forall g b xs.
  Seq.fsts (joinD g b xs) = joinD g b (mapD g Seq.fsts xs)

"Seq.snds/joinD" forall g b xs.
  Seq.snds (joinD g b xs) = joinD g b (mapD g Seq.snds xs)

"Seq.fsts/splitJoinD" forall g f xs.
  Seq.fsts (splitJoinD g f xs) = splitJoinD g (mapD g Seq.fsts . f) xs

"Seq.snds/splitJoinD" forall g f xs.
  Seq.snds (splitJoinD g f xs) = splitJoinD g (mapD g Seq.snds . f) xs
-}

{-#

"Seq.zip/joinD[1]" forall g xs ys.
  Seq.zip (joinD g balanced xs) ys
    = joinD g balanced (zipWithD g Seq.zip xs (splitD g balanced ys))

"Seq.zip/joinD[2]" forall g xs ys.
  Seq.zip xs (joinD g balanced ys)
    = joinD g balanced (zipWithD g Seq.zip (splitD g balanced xs) ys)

"Seq.zip/splitJoinD" forall gang f g xs ys.
  Seq.zip (splitJoinD gang (imapD gang f) xs) (splitJoinD gang (imapD gang g) ys)
    = splitJoinD gang (imapD gang (\i zs -> let (as,bs) = Seq.unzip zs
                                            in Seq.zip (f i as) (g i bs)))
                      (Seq.zip xs ys)

  #-}

-- | Permute for distributed arrays.
permuteD :: forall a. Unbox a => Gang -> Dist (Vector a) -> Dist (Vector Int) -> Vector a
{-# INLINE_DIST permuteD #-}
permuteD g darr dis = Seq.new n (\ma -> zipWithDST_ g (permute ma) darr dis)
  where
    n = joinLengthD g darr
    permute :: forall s. MVector s a -> Vector a -> Vector Int -> DistST s ()
    permute ma arr is = stToDistST (Seq.mpermute ma arr is)


-- NOTE: The bang is necessary because the array must be fully evaluated
-- before we pass it to the parallel computation.
bpermuteD :: Unbox a => Gang -> Vector a -> Dist (Vector Int) -> Dist (Vector a)
{-# INLINE bpermuteD #-}
bpermuteD g !as ds = mapD g (Seq.bpermute as) ds

-- NB: This does not (and cannot) try to prevent two threads from writing to
-- the same position. We probably want to consider this an (unchecked) user
-- error.
atomicUpdateD :: forall a. Unbox a
             => Gang -> Dist (Vector a) -> Dist (Vector (Int,a)) -> Vector a
{-# INLINE atomicUpdateD #-}
atomicUpdateD g darr upd = runST (
  do
    marr <- joinDM g darr
    mapDST_ g (update marr) upd
    Seq.unsafeFreeze marr
  )
  where
    update :: forall s. MVector s a -> Vector (Int,a) -> DistST s ()
    update marr arr = stToDistST (Seq.mupdate marr arr)

splitSegdD :: Gang -> USegd -> Dist USegd
{-# NOINLINE splitSegdD #-}
splitSegdD g !segd = mapD g lengthsToUSegd
                   $ splitAsD g d lens
  where
    !d = snd
       . mapAccumLD g chunk 0
       . splitLenD g
       $ elementsUSegd segd

    n = lengthUSegd segd
    lens = lengthsUSegd segd

    chunk !i !k = let !j = go i k
                  in (j,j-i)

    go !i !k | i >= n    = i
             | m == 0    = go (i+1) k
             | k <= 0    = i
             | otherwise = go (i+1) (k-m)
      where
        m = lens ! i


search :: Int -> Vector Int -> Int
search !x ys = go 0 (Seq.length ys)
  where
    go i n | n <= 0        = i
           | (ys!mid) < x = go (mid+1) (n-half-1)
           | otherwise     = go i half
      where
        half = n `shiftR` 1
        mid  = i + half

chunk :: USegd -> Int -> Int -> Bool -> (# Vector Int, Int, Int #)
chunk !segd !di !dn is_last
  = (# lens', k-left_len, left_off #)
  where
    !lens' = runST (do
                      mlens' <- Seq.newM n'
                      when (left /= 0) $ Seq.write mlens' 0 left
                      Seq.copy (Seq.mdrop left_len mlens')
                               (Seq.slice lens k (k'-k))
                      when (right /= 0) $ Seq.write mlens' (n' - 1) right
                      Seq.unsafeFreeze mlens')

    lens = lengthsUSegd segd
    idxs = indicesUSegd segd
    n    = Seq.length lens

    k  = search di idxs
    k' | is_last   = n
       | otherwise = search (di+dn) idxs

    left  | k == n    = dn
          | otherwise = min ((idxs!k) - di) dn

    right | k' == k   = 0
          | otherwise = di + dn - (idxs ! (k'-1))

    left_len | left == 0   = 0
             | otherwise   = 1

    left_off | left == 0   = 0
             | otherwise   = di - idxs ! (k-1)

    n' = left_len + (k'-k)

splitSegdD' :: Gang -> USegd -> Dist ((USegd,Int),Int)
{-# INLINE splitSegdD' #-}
splitSegdD' g !segd = imapD g mk
                         (splitLenIdxD g
                         (elementsUSegd segd))
  where
    !p = gangSize g

    mk i (dn,di) = case chunk segd di dn (i == p-1) of
                     (# lens, l, o #) -> ((lengthsToUSegd lens,l),o)

joinSegD :: Gang -> Dist USegd -> USegd
{-# INLINE_DIST joinSegD #-}
joinSegD g = lengthsToUSegd
           . joinD g unbalanced
           . mapD g lengthsUSegd

splitSD :: Unbox a => Gang -> Dist USegd -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitSD #-}
splitSD g dsegd xs = splitAsD g (elementsUSegdD dsegd) xs

{-# RULES

"splitSD/splitJoinD" forall g d f xs.
  splitSD g d (splitJoinD g f xs) = f (splitSD g d xs)

"splitSD/Seq.zip" forall g d xs ys.
  splitSD g d (Seq.zip xs ys) = zipWithD g Seq.zip (splitSD g d xs)
                                             (splitSD g d ys)

  #-}

