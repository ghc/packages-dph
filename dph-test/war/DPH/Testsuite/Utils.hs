{-# LANGUAGE TypeSynonymInstances, 
        GeneralizedNewtypeDeriving #-}

module Testsuite.Utils (
  Len(..), Perm(..), SizedInt(..),
  
  Proxy(..), asProxyTypeOf,
    
  segdForArray, checkSegd,
  
  limitRange, update, nest
) where

import Test.QuickCheck

import Text.Show.Functions

import Data.Array.Parallel.Unlifted as U hiding ( update )
import Prelude as P

import Data.List ( delete, sort )

import System.Random ( StdGen, mkStdGen)

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


-- Segment descriptor of length n.
-- 
-- Do not use directly unless an arbitrary Segd is all you need.  
-- Use segdForArray to generate a Segd that fits the array.
instance Arbitrary Segd where
  arbitrary = sized $ \n -> 
    do
      ids <- genIndices n
      let lens = indicesToLengths ids n
      return $ mkSegd (fromList lens) (fromList ids) n
    where 
      -- list of non-decreasing integers in range [0, n)
      genIndices 0 = return []
      genIndices n = ((0:) . sort . P.map (`mod` n)) `fmap` arbitrary
      indicesToLengths ids n = P.zipWith (-) (tail $ ids ++ [n]) ids

-- Generate segment descriptor fitting the given array
segdForArray :: (Elt a) => Array a -> Gen Segd
segdForArray arr = resize (U.length arr) arbitrary

-- Consistency check for a segment descriptor against a provided list of lengths
checkSegd :: Segd -> Array Int -> Bool
checkSegd segd lens =
     (lengthsSegd  segd == lens)
  && (indicesSegd  segd == U.scan (+) 0 lens)
  && (elementsSegd segd == U.sum lens)

-- Random number generator
instance Arbitrary StdGen where
  arbitrary = mkStdGen `fmap` arbitrary


-- A phantom type similar to the one found in lib `tagged'
data Proxy a = Proxy deriving (Show)

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const

------------------------------- Helper functins --------------------------------
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

-- Nest elements of an array according to segments lengths descriptor
nest :: [Int] -> [a] -> [[a]]
nest (n : ns) xs = let (ys, zs) = P.splitAt n xs
                   in ys : nest ns zs
nest _ _ = []

