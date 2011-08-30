{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Operations on distributed arrays.
module Data.Array.Parallel.Unlifted.Distributed.Arrays (
  -- * Distribution phantom parameter
  Distribution, balanced, unbalanced,

  -- * Array Lengths
  lengthD, splitLenD, splitLenIdxD,
  
  splitAsD, splitD, joinLengthD, joinD, splitJoinD, joinDM,
  splitSegdD, splitSegdD', joinSegdD, splitSD,

  permuteD, bpermuteD, atomicUpdateD

) where
import Data.Array.Parallel.Base ( ST, runST)
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Unlifted.Distributed.DistST
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Scalars
import Data.Array.Parallel.Unlifted.Sequential.Vector   (Vector, MVector, Unbox, (!))
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Bits     ( shiftR )
import Control.Monad ( when )
import GHC.Base      ( quotInt, remInt )

here s = "Data.Array.Parallel.Unlifted.Distributed.Arrays." Prelude.++ s


-- Distribution ---------------------------------------------------------------
-- | This is a phantom parameter used to record whether a distributed value
--   is balanced evenly among the threads. It's used to signal this property
--   between RULES, but the actual value is never used.
data Distribution

balanced :: Distribution
{-# NOINLINE balanced #-}
balanced   = error $ here "balanced: touched"

unbalanced :: Distribution
{-# NOINLINE unbalanced #-}
unbalanced = error $ here "unbalanced: touched"


-- Splitting and Joining array lengths ----------------------------------------
-- | O(threads).
--   Distribute an array length over a 'Gang'.
--   Each thread holds the number of elements it's reponsible for.
--   If the array length doesn't split evenly among the threads then the first
--   threads get a few more elements.
--
--   @splitLenD theGangN4 511
--      = [128,128,128,127]@
-- 
splitLenD :: Gang -> Int -> Dist Int
{-# INLINE splitLenD #-}
splitLenD g n = generateD_cheap g len
  where
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt`  p

    {-# INLINE [0] len #-}
    len i | i < m     = l+1
          | otherwise = l


-- | O(threads).
--   Distribute an array length over a 'Gang'.
--   Each thread holds the number of elements it's responsible for, 
--   and the index of the start of its chunk.
--
--   @splitLenIdxD theGangN4 511 
--      = [(128,0),(128,128),(128,256),(127,384)]@
--
splitLenIdxD :: Gang -> Int -> Dist (Int, Int)
{-# INLINE splitLenIdxD #-}
splitLenIdxD g n = generateD_cheap g len_idx
  where
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    {-# INLINE [0] len_idx #-}
    len_idx i | i < m     = (l+1, i*(l+1))
              | otherwise = (l,   i*l + m)


-- | O(threads).
--   Get the overall length of a distributed array.
--   This is implemented by reading the chunk length from each thread, 
--   and summing them up.
joinLengthD :: Unbox a => Gang -> Dist (Vector a) -> Int
{-# INLINE joinLengthD #-}
joinLengthD g = sumD g . lengthD
                                               

-- Splitting and Joining arrays -----------------------------------------------
-- | Distribute an array over a 'Gang' such that each threads gets the given
--   number of elements.
--
--   @splitAsD theGangN4 (splitLenD theGangN4 10) [1 2 3 4 5 6 7 8 9 0]
--      = [[1 2 3] [4 5 6] [7 8] [9 0]]@
-- 
splitAsD :: Unbox a => Gang -> Dist Int -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitAsD #-}
splitAsD g dlen !arr 
  = zipWithD (seqGang g) (Seq.slice arr) is dlen
  where
    is = fst $ scanD g (+) 0 dlen


-- | Distribute an array over a 'Gang'.
--
--   NOTE: This is defined in terms of splitD_impl to avoid introducing loops
--         through RULES. Without it, splitJoinD would be a loop breaker.
-- 
splitD :: Unbox a => Gang -> Distribution -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitD #-}
splitD g _ arr = splitD_impl g arr

splitD_impl :: Unbox a => Gang -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitD_impl #-}
splitD_impl g !arr 
  = generateD_cheap g (\i -> Seq.slice arr (idx i) (len i))
  where
    n  = Seq.length arr
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    {-# INLINE [0] idx #-}
    idx i | i < m     = (l+1)*i
          | otherwise = l*i + m

    {-# INLINE [0] len #-}
    len i | i < m     = l+1
          | otherwise = l


-- | Join a distributed array.
--   Join sums up the array lengths of each chunk, allocates a new result array, 
--   and copies each chunk into the result.
--
--   NOTE: This is defined in terms of joinD_impl to avoid introducing loops
--         through RULES. Without it, splitJoinD would be a loop breaker.
--
joinD :: Unbox a => Gang -> Distribution -> Dist (Vector a) -> Vector a
{-# INLINE CONLIKE [1] joinD #-}
joinD g _ darr  = joinD_impl g darr

joinD_impl :: forall a. Unbox a => Gang -> Dist (Vector a) -> Vector a
{-# INLINE_DIST joinD_impl #-}
joinD_impl g !darr 
  = checkGangD (here "joinD") g darr 
  $ Seq.new n (\ma -> zipWithDST_ g (copy ma) di darr)
  where
    (!di,!n)      = scanD g (+) 0 $ lengthD darr

    copy :: forall s. MVector s a -> Int -> Vector a -> DistST s ()
    copy ma i arr = stToDistST (Seq.copy (Seq.mslice i (Seq.length arr) ma) arr)


-- | Split a vector over a gang, run a distributed computation, then
--   join the pieces together again.
splitJoinD
        :: (Unbox a, Unbox b)
        => Gang
        -> (Dist (Vector a) -> Dist (Vector b))
        -> Vector a
        -> Vector b
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
    copy ma i arr = stToDistST (Seq.copy (Seq.mslice i (Seq.length arr) ma) arr)


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

  #-}

{-# RULES

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


-- Permutation ----------------------------------------------------------------
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


-- Update ---------------------------------------------------------------------
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


--- Splitting and Joining segment descriptors ---------------------------------
splitSegdD :: Gang -> USegd -> Dist USegd
{-# NOINLINE splitSegdD #-}
splitSegdD g !segd = mapD g lengthsToUSegd
                   $ splitAsD g d lens
  where
    !d = snd
       . mapAccumLD g chunks 0
       . splitLenD g
       $ elementsUSegd segd

    n    = lengthUSegd segd
    lens = lengthsUSegd segd

    chunks !i !k = let !j = go i k
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



joinSegdD :: Gang -> Dist USegd -> USegd
{-# INLINE_DIST joinSegdD #-}
joinSegdD g = lengthsToUSegd
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
