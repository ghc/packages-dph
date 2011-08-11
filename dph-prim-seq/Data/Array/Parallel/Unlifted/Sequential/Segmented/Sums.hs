-- | Sum-like operations on segmented list-like combinators.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.Sums (
  andSU, orSU, sumSU, productSU, maximumSU, minimumSU,
  sumRU
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (
  USegd )
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators (
  foldSU, fold1SU, foldlRU)


-- | Compute the boolean AND of all segments in a segmented array.
andSU :: USegd -> Vector Bool -> Vector Bool
andSU = foldSU (&&) True


-- | Compute the boolean OR of all segments in a segmented array.
orSU :: USegd -> Vector Bool -> Vector Bool
orSU = foldSU (||) False


-- | Compute the segmented sum of an array of numerals
sumSU :: (Num e, Unbox e) => USegd -> Vector e -> Vector e
{-# INLINE sumSU #-}
sumSU = foldSU (+) 0


-- | Compute the segmented product of an array of numerals
productSU :: (Num e, Unbox e) => USegd -> Vector e -> Vector e
{-# INLINE productSU #-}
productSU = foldSU (*) 1


-- | Determine the maximum element in each subarray
maximumSU :: (Ord e, Unbox e) => USegd -> Vector e -> Vector e
{-# INLINE maximumSU #-}
maximumSU = fold1SU max


-- | Determine the minimum element in each subarray
minimumSU :: (Ord e, Unbox e) => USegd -> Vector e -> Vector e
{-# INLINE minimumSU #-}
minimumSU = fold1SU min


-- | Compute the segmented sum of an array of numerals
sumRU :: (Num e, Unbox e) => Int ->Vector e -> Vector e
{-# INLINE sumRU #-}
sumRU = foldlRU (+) 0
