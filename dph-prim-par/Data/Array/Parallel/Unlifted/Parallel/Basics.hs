{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Basic operations on parallel unlifted arrays.
module Data.Array.Parallel.Unlifted.Parallel.Basics (
  emptyUP,
  replicateUP,
  repeatUP,
  lengthUP,
  nullUP,
  interleaveUP,
  indexedUP
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators ( mapUP )
import Data.Array.Parallel.Unlifted.Parallel.Enum        ( enumFromToUP )
import Data.Array.Parallel.Unlifted.Parallel.Permute     ( bpermuteUP )
import GHC.Base ( remInt )


-- | O(1). Construct an empty array.
emptyUP :: Unbox e => Vector e
{-# INLINE_UP emptyUP #-}
emptyUP = Seq.new 0 (const $ return ())


-- | Yield an array where all elements contain the same value
replicateUP :: Unbox e => Int -> e -> Vector e
{-# INLINE_UP replicateUP #-}
replicateUP n !e 
        = joinD theGang balanced
        . mapD theGang (\n ->Seq.replicate n e)
        $ splitLenD theGang n


-- | Repeat an array the given number of times.
repeatUP :: Unbox e => Int -> Vector e -> Vector e
{-# INLINE_UP repeatUP #-}
repeatUP n es 
        = seq m
        . bpermuteUP es
        . mapUP (\i -> i `remInt` m)
        $ enumFromToUP 0 (m*n-1)
  where
    m = Seq.length es

-- | O(1). Take the length of an array.
lengthUP :: Unbox e => Vector e -> Int
{-# INLINE_UP lengthUP #-}
lengthUP = Seq.length


-- | O(1). Test whether the given array is empty
nullUP :: Unbox e => Vector e -> Bool
{-# INLINE_UP nullUP #-}
nullUP  = (== 0) . Seq.length


-- | Interleave elements of two arrays
interleaveUP :: Unbox e => Vector e -> Vector e -> Vector e
{-# INLINE_UP interleaveUP #-}
interleaveUP xs ys
        = joinD theGang unbalanced
        $ zipWithD theGang Seq.interleave
                (splitD theGang balanced xs)
                (splitD theGang balanced ys)


-- | Associate each element of the array with its index
indexedUP :: (DT e, Unbox e) => Vector e -> Vector (Int,e)
{-# INLINE_UP indexedUP #-}
indexedUP 
 = splitJoinD theGang indexedFn 
 where
    sizes  arr   = fst $ scanD theGang (+) 0 $ lengthD arr
    indexedFn    = \arr -> zipWithD theGang (\o -> Seq.map (\(x,y) -> (x + o, y))) (sizes arr) 
                        $  mapD theGang Seq.indexed arr
