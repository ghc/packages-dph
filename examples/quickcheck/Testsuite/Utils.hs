module Testsuite.Utils (
  Perm(..)
) where

import Test.QuickCheck

import Text.Show.Functions

import Data.Array.Parallel.Unlifted as U
import Prelude as P

import Data.List ( permutations )


instance (Elt a, Arbitrary a) => Arbitrary (Array a) where
  arbitrary = fmap fromList arbitrary


-- non-negative integer
newtype Len = Len Int deriving (Eq,Ord,Enum,Show,Num)

instance Arbitrary Len where
  arbitrary = sized $ \n -> Len `fmap` choose (0,n)


-- permutation of [0..n-1] with all values appearing exactly once
newtype Perm = Perm (Array Int) deriving (Eq,Show)

instance Arbitrary Perm where
  arbitrary   = Perm `fmap` (sized $ \n -> elements $ P.map fromList (permutations [0..n-1]))

