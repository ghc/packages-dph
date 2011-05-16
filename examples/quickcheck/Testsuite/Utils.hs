module Testsuite.Utils (
  Len(..), Perm(..), BPerm(..), DftPerm(..)

--gvector, gdist, gtype, vtype
) where

import Test.QuickCheck
import Test.QuickCheck.Batch

import Text.Show.Functions

import Data.Array.Parallel.Unlifted as U

import Data.Char
import Data.List ( permutations )
import Control.Monad (liftM)
import Prelude as P

-- infix 4 ===

newtype Len = Len Int deriving (Eq,Ord,Enum,Show,Num)

-- permutation of [0..n-1] with all values appearing exactly once
newtype Perm = Perm (Array Int) deriving (Eq,Show)

-- array of elements taken from [0..n-1]
newtype BPerm = BPerm (Array Int) deriving (Eq,Show)

-- array of index-value pairs with indices taken from [0..n-1]
newtype DftPerm a = DftPerm (Array (Int, a)) deriving (Eq, Show)

instance Arbitrary Char where
  arbitrary   = fmap chr . sized $ \n -> choose (0,n)
  coarbitrary = coarbitrary . ord

{-
instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
  arbitrary = liftM (uncurry (:*:)) arbitrary
  coarbitrary (a :*: b) = coarbitrary (a,b)
-}

instance Arbitrary Len where
  arbitrary = sized $ \n -> Len `fmap` choose (0,n)
  coarbitrary (Len n) = coarbitrary n

instance Arbitrary Perm where
  arbitrary   = Perm `fmap` (sized $ \n -> elements $ P.map fromList (permutations [0..n-1]))
  coarbitrary = \(Perm arr) -> coarbitrary (toList arr)

instance Arbitrary BPerm where
  arbitrary   = sized $ \n -> (BPerm . fromList . P.map (`mod` n)) `fmap` enlarge n arbitrary
  coarbitrary = \(BPerm arr) -> coarbitrary (toList arr)
  
instance (Elt a, Arbitrary a) => Arbitrary (DftPerm a) where
  arbitrary   = do
                  BPerm idxs <- arbitrary                        -- :: Gen BPerm
                  vals       <- sized $ \n -> enlarge n arbitrary -- :: Gen (Array a)
                  return $ DftPerm (U.zip idxs vals)
  coarbitrary =   \(DftPerm arr) -> coarbitrary (toList arr)

{-
instance Arbitrary a => Arbitrary (MaybeS a) where
  arbitrary = frequency [(1, return NothingS), (3, liftM JustS arbitrary)]
  coarbitrary NothingS  = variant 0
  coarbitrary (JustS x) = variant 1 . coarbitrary x
-}

instance (Elt a, Arbitrary a) => Arbitrary (Array a) where
  arbitrary = fmap fromList arbitrary
  coarbitrary = coarbitrary . toList

{-
instance (UA a, Arbitrary a) => Arbitrary (SUArr a) where
  arbitrary   = fmap toSU arbitrary
  coarbitrary = coarbitrary . fromSU

instance Arbitrary Gang where
  arbitrary = sized $ \n -> sequentialGang `fmap` choose (1,n+1)
  coarbitrary = coarbitrary . gangSize
-}

-- wrapper to Test.QuickCheck.resize facilitating the generation of
-- longer permutations while keeping indices within bounds
enlarge n = resize $ case n of 0 -> 0    -- potential corner cases
                               1 -> 1    -- these too
                               n -> 2*n
{-
gvector :: Arbitrary a => Gang -> Gen [a]
gvector = vector . gangSize

gdist :: (Arbitrary a, DT a) => Gang -> Gen (Dist a)
gdist g = sized $ \n -> resize (n `div` gangSize g + 1) $ toD g `fmap` gvector g

vtype :: Gen [a] -> a -> Gen [a]
vtype = const

gtype :: Gen (Dist a) -> a -> Gen (Dist a)
gtype = const
-}

{-
class Eq a => SemEq a where
  (===) :: a -> a -> Bool

instance Eq a => SemEq a where
  x === y | isBottom x = isBottom y
          | isBottom y = False
          | otherwise  = x == y
-}

