{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Operations on Distributed Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.USegd (
        splitSegdD, splitSegdD', splitSD,
        joinSegdD
)
where
import Data.Array.Parallel.Unlifted.Distributed.Arrays
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector, Unbox, (!))
import Data.Array.Parallel.Base
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Bits     ( shiftR )
import Control.Monad ( when )


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
