{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Segmented (
  replicateRSUP, appendSUP,
  foldRUP,
  sumRUP
) where

import Data.Array.Parallel.Unlifted.Distributed

import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Sums
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd            (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd             (UPSegd)
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd  as UPSSegd

import Data.Array.Parallel.Unlifted.Sequential.Basics           as Seq
import Data.Array.Parallel.Unlifted.Sequential.Combinators      as Seq
import Data.Array.Parallel.Unlifted.Sequential.USegd            as Seq
import Data.Array.Parallel.Unlifted.Sequential.Vector           as Seq

import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Stream.Size    ( Size(..) )
import qualified Data.Vector.Fusion.Stream              as S
import qualified Data.Vector                            as V
import Control.Monad.ST ( ST, runST )


-- replicate ------------------------------------------------------------------

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
        = UPSegd.replicateWith (UPSegd.fromLengths (replicateUP (Seq.length xs) n)) xs


-- Append ---------------------------------------------------------------------
-- | Segmented append.
appendSUP
        :: Unbox a
        => UPSegd               -- ^ segment descriptor of result array
        -> UPSegd -> Vector a   -- ^ segd/data of first array
        -> UPSegd -> Vector a   -- ^ segd/data of second array
        -> Vector a

{-# INLINE_UP appendSUP #-}
appendSUP segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD  theGang append
  $ UPSegd.takeDistributed segd
  where append ((segd,seg_off),el_off)
         = Seq.unstream
         $ appendSegS (UPSegd.takeUSegd xd) xs
                      (UPSegd.takeUSegd yd) ys
                      (elementsUSegd segd)
                      seg_off el_off

-- append ---------------------------------------------------------------------
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


-- foldR ----------------------------------------------------------------------
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


-- sumR -----------------------------------------------------------------------
sumRUP :: (Num e, Unbox e) => Int -> Vector e -> Vector e
{-# INLINE sumRUP #-}
sumRUP = foldRUP (+) 0


