module Testsuite.Utils (
  Len(..), Perm(..), SizedInt(..), limitRange, update
) where

import Test.QuickCheck

import Text.Show.Functions

import Data.Array.Parallel.Unlifted as U hiding ( update )
import Prelude as P

import Data.List ( delete )

--------------------------- Test data generators ----------------------------

instance (Elt a, Arbitrary a) => Arbitrary (Array a) where
  arbitrary = fmap fromList arbitrary


-- non-negative integer
newtype Len = Len Int deriving (Eq, Ord, Enum, Show, Num)

instance Arbitrary Len where
  arbitrary = sized $ \n -> Len `fmap` choose (0,n)


-- permutation of [0..n-1] with all values appearing exactly once
newtype Perm = Perm (Array Int) deriving (Eq, Show)

instance Arbitrary Perm where
  arbitrary = sized $ \n -> (Perm . fromList) `fmap` (permute [0..n-1])
    where -- generate a random permutation of the given list
      permute :: [Int] -> Gen [Int]
      permute [] = return []
      permute xs = do 
                   -- choose random element of the list and place it at head
                      x    <- elements xs
                      rest <- permute (delete x xs)
                      return (x : rest)


-- integer whose max absolute value depends on the size parameter
newtype SizedInt = SizedInt Int deriving (Eq, Ord, Enum, Show, Num)

instance Arbitrary SizedInt where
  arbitrary = SizedInt `fmap` arbitrarySizedIntegral

------------------------------ Helper functins ---------------------------------
-- TODO: Enhance TH testing infrastructure to allow keeping helper fuctions in
--       in the same files as the properties.
  
-- Adjust elements of an array to be within range [0, n).
-- As a special case, returns an empty array if the length is 0.
limitRange :: Int -> Array Int -> Array Int
limitRange n ixs = if n <= 0 then empty -- no permutations for an empty array
                             else U.map (`mod` n) ixs

-- Update specified list elements using index/value pairs
update :: [a] -> [(Int, a)] -> [a]
update xs []              = xs
update xs ((i,x) : pairs) = update (set xs i x) pairs
    where set xs i x = (P.take i xs) ++ [x] ++ (P.drop (i+1) xs)

