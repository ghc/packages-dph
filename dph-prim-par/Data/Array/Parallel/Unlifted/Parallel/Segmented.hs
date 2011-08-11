{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Segmented (
  replicateSUP, replicateRSUP, appendSUP, indicesSUP,
  foldSUP, foldRUP, fold1SUP, sumSUP, sumRUP
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP, zipWithUP, packUP, combineUP)
import Data.Array.Parallel.Unlifted.Parallel.Sums (
  sumUP )
import Data.Array.Parallel.Unlifted.Parallel.Basics (
  replicateUP, repeatUP)
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute ( bpermuteUP )
import Data.Array.Parallel.Unlifted.Parallel.UPSegd
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Stream.Size    ( Size(..) )
import Control.Monad.ST ( ST, runST )


-- replicate ------------------------------------------------------------------

-- | Segmented replication, using a segment descriptor.
replicateSUP :: Unbox a => UPSegd -> Vector a -> Vector a
{-# INLINE_UP replicateSUP #-}
replicateSUP segd !xs 
  = joinD theGang balanced
  . mapD theGang rep
  $ distUPSegd segd
  where
    rep ((dsegd,di),_)
      = replicateSU dsegd (Seq.slice xs di (lengthUSegd dsegd))


-- | Segmented replication.
--   Each element in the vector is replicated the given number of times.
--   
--   @replicateRSUP 2 [1, 2, 3, 4, 5] = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]@
--
--   TODO: make this efficient
-- 
replicateRSUP :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE_UP replicateRSUP #-}
replicateRSUP n xs
        = replicateSUP (lengthsToUPSegd (replicateUP (Seq.length xs) n)) xs


-- append ---------------------------------------------------------------------
-- | Segmented append.
appendSUP
        :: Unbox a
        => UPSegd       -- ^ segment descriptor of result array
        -> UPSegd       -- ^ segment descriptor of first array
        -> Vector a     -- ^ data of first array
        -> UPSegd       -- ^ segment descriptor of second array
        -> Vector a     -- ^ data of first array
        -> Vector a

{-# INLINE_UP appendSUP #-}
appendSUP segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD  theGang append
  $ distUPSegd segd
  where append ((segd,seg_off),el_off)
         = Seq.unstream
         $ appendSegS (segdUPSegd xd) xs
                      (segdUPSegd yd) ys
                      (elementsUSegd segd)
                      seg_off el_off


appendSegS
        :: Unbox a      
        => USegd        -- ^ segment descriptor of first array
        -> Vector a     -- ^ data of first array
        -> USegd        -- ^ segment descriptor of second array
        -> Vector a     -- ^ data of second array
        -> Int          -- 
        -> Int
        -> Int
        -> S.Stream a

{-# INLINE_STREAM appendSegS #-}
appendSegS !xd !xs !yd !ys !n seg_off el_off
  = Stream next state (Exact n)
  where
    !xlens = lengthsUSegd xd
    !ylens = lengthsUSegd yd

    state
      | n == 0 = Nothing
      | el_off < xlens ! seg_off
      = let i = (indicesUSegd xd ! seg_off) + el_off
            j = indicesUSegd yd ! seg_off
            k = (lengthsUSegd xd ! seg_off) - el_off
        in  Just (False, seg_off, i, j, k, n)

      | otherwise
      = let -- NOTE: *not* indicesUSegd xd ! (seg_off+1) since seg_off+1
            -- might be out of bounds
            i       = (indicesUSegd xd ! seg_off) + (lengthsUSegd xd ! seg_off)
            el_off' = el_off - lengthsUSegd xd ! seg_off
            j       = (indicesUSegd yd ! seg_off) + el_off'
            k       = (lengthsUSegd yd ! seg_off) - el_off'
        in  Just (True, seg_off, i, j, k, n)

    {-# INLINE next #-}
    next Nothing = return Done

    next (Just (False, seg, i, j, k, n))
      | n == 0    = return Done
      | k == 0    = return $ Skip (Just (True, seg, i, j, ylens ! seg, n))
      | otherwise = return $ Yield (xs!i) (Just (False, seg, i+1, j, k-1, n-1))

    next (Just (True, seg, i, j, k, n))
      | n == 0    = return Done
      | k == 0
      = let !seg' = seg+1
        in  return $ Skip (Just (False, seg', i, j, xlens ! seg', n))

      | otherwise = return $ Yield (ys!j) (Just (True, seg, i, j+1, k-1, n-1))


-- fold -----------------------------------------------------------------------
fixupFold :: Unbox a => (a -> a -> a) -> MVector s a
          -> Dist (Int,Vector a) -> ST s ()
{-# NOINLINE fixupFold #-}
fixupFold f !mrs !dcarry = go 1
  where
    !p = gangSize theGang

    go i | i >= p = return ()
         | Seq.null c = go (i+1)
         | otherwise   = do
                           x <- Seq.read mrs k
                           Seq.write mrs k (f x (c ! 0))
                           go (i+1)
      where
        (k,c) = indexD dcarry i


folds :: Unbox a => (a -> a -> a)
              -> (USegd -> Vector a -> Vector a) -> UPSegd -> Vector a -> Vector a
{-# INLINE folds #-}
folds f g segd xs = dcarry `seq` drs `seq` runST (
  do
    mrs <- joinDM theGang drs
    fixupFold f mrs dcarry
    Seq.unsafeFreeze mrs)
  where
    (dcarry,drs)
          = unzipD
          $ mapD theGang partial
          $ zipD (distUPSegd segd)
                 (splitD theGang balanced xs)

    partial (((segd,k),off), as)
      = let rs = g segd as
            {-# INLINE [0] n #-}
            n | off == 0  = 0
              | otherwise = 1
        in
        ((k, Seq.take n rs), Seq.drop n rs)


foldSUP :: Unbox a => (a -> a -> a) -> a -> UPSegd -> Vector a -> Vector a
{-# INLINE foldSUP #-}
foldSUP f !z = folds f (foldlSU f z)


fold1SUP :: Unbox a => (a -> a -> a) -> UPSegd -> Vector a -> Vector a
{-# INLINE fold1SUP #-}
fold1SUP f = folds f (fold1SU f)


sumSUP :: (Num e, Unbox e) => UPSegd -> Vector e -> Vector e
{-# INLINE sumSUP #-}
sumSUP = foldSUP (+) 0


sumRUP :: (Num e, Unbox e) => Int -> Vector e -> Vector e
{-# INLINE sumRUP #-}
sumRUP = foldRUP (+) 0


foldRUP :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
{-# INLINE foldRUP #-}
foldRUP f z !segSize xs = 
   joinD theGang unbalanced
    (mapD theGang 
      (foldlRU f z segSize)
      (splitAsD theGang (mapD theGang (*segSize) dlen) xs))
  where
    noOfSegs = Seq.length xs `div` segSize
    dlen = splitLenD theGang noOfSegs


-- indices --------------------------------------------------------------------
indicesSUP :: UPSegd -> Vector Int
{-# INLINE_UP indicesSUP #-}
indicesSUP = joinD theGang balanced
           . mapD theGang indices
           . distUPSegd
  where
    indices ((segd,k),off) = indicesSU' off segd

