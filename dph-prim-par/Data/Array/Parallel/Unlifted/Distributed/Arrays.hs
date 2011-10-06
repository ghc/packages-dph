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
  
  -- * Splitting and joining
  splitAsD, splitD, joinLengthD, joinD, splitJoinD, joinDM,

  -- * Permutations
  permuteD, bpermuteD,
  
  -- * Update
  atomicUpdateD,
  
  -- * Carry
  carryD

) where
import Data.Array.Parallel.Base (ST, runST)
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Unlifted.Distributed.DistST
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Scalars
import Data.Array.Parallel.Unlifted.Sequential.Vector   (Vector, MVector, Unbox)
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import GHC.Base      ( quotInt, remInt )
import Control.Monad

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


-- Carry ----------------------------------------------------------------------
-- | Selectively combine the last elements of some chunks with the
--   first elements of others.
--
--   NOTE: This runs sequentially and should only be used for testing purposes.
--
--   @   pprp $ splitD theGang unbalanced $ fromList [80, 10, 20, 40, 50, 10 :: Int]
--       DVector lengths: [2,2,1,1]
--       chunks:  [[80,10],[20,40],[50],[10]]
-- 
--       pprp $ fst 
--            $ carryD theGang (+) 0 
--                     (mkDPrim $ fromList [True, False, True, False]) 
--                     (splitD theGang unbalanced $ fromList [80, 10, 20, 40, 50, 10 :: Int])
--
--        DVector lengths: [1,2,0,1]
--                chunks:  [[80],[30,40],[],[60]]
--   @
--
carryD  :: forall a
        .  (Unbox a, DT a)
        => Gang 
        -> (a -> a -> a) -> a
        -> Dist Bool
        -> Dist (Vector a)
        -> (Dist (Vector a), a)

carryD gang f zero shouldCarry vec
 = runST (do
        md      <- newMD gang
        acc     <- carryD' f zero shouldCarry vec md
        d       <- unsafeFreezeMD md
        return (d, acc))


carryD' :: forall a s
        .  (Unbox a, DT a)
        => (a -> a -> a) -> a
        -> Dist Bool
        -> Dist (Vector a)
        -> MDist (Vector a) s
        -> ST s a

carryD' f zero shouldCarry vec md_
 = go md_ zero 0
 where go (md :: MDist (Vector a) s) prev ix
        | ix >= sizeD vec    = return prev
        | otherwise
        = do let chunk :: Vector a
                 !chunk      = indexD vec ix
             let !chunkLen   = Seq.length chunk

             -- Whether to carry the last value of this chunk into the next chunk
             let !carry      = indexD shouldCarry ix

             -- The new length for this chunk
             let !chunkLen'  
                   | chunkLen == 0 = 0
                   | carry         = chunkLen - 1
                   | otherwise     = chunkLen

             -- The new value of the accumulator
             let acc            = f prev (chunk Seq.! 0)
                
             -- Allocate a mutable vector to hold the new chunk and copy
             -- source elements into it.
             mchunk' <- Seq.newM chunkLen'
             Seq.copy mchunk' (Seq.slice chunk 0 chunkLen')

             when (chunkLen' /= 0)
              $ Seq.write mchunk' 0 acc

             -- Store the new chunk in the gang
             chunk'  <- Seq.unsafeFreeze mchunk'
             writeMD md ix chunk'

             -- What value to carry into the next chunk
             let next
                  | chunkLen' == 0      = acc
                  | carry               = chunk Seq.! (chunkLen - 1)
                  | otherwise           = zero
                               
             go md next (ix + 1)

