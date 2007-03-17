module Testsuite.Utils (
  Len(..), EFL,

  gvector, gdist, gtype, vtype
) where

import Test.QuickCheck
import Test.QuickCheck.Batch

import Text.Show.Functions

import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Base.Fusion       (EFL)
import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Distributed

import Data.Char
import Monad (liftM)

-- infix 4 ===

newtype Len = Len Int deriving(Eq,Ord,Enum,Show,Num)

instance Arbitrary Char where
  arbitrary   = fmap chr . sized $ \n -> choose (0,n)
  coarbitrary = coarbitrary . ord

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
  arbitrary = liftM (uncurry (:*:)) arbitrary
  coarbitrary (a :*: b) = coarbitrary (a,b)

instance Arbitrary Len where
  arbitrary = sized $ \n -> Len `fmap` choose (0,n)
  coarbitrary (Len n) = coarbitrary n

instance Arbitrary a => Arbitrary (MaybeS a) where
  arbitrary = frequency [(1, return NothingS), (3, liftM JustS arbitrary)]
  coarbitrary NothingS  = variant 0
  coarbitrary (JustS x) = variant 1 . coarbitrary x

instance (UA a, Arbitrary a) => Arbitrary (UArr a) where
  arbitrary = fmap toU arbitrary
  coarbitrary = coarbitrary . fromU

instance (UA a, Arbitrary a) => Arbitrary (SUArr a) where
  arbitrary   = fmap toSU arbitrary
  coarbitrary = coarbitrary . fromSU

instance Arbitrary Gang where
  arbitrary = sized $ \n -> sequentialGang `fmap` choose (1,n+1)
  coarbitrary = coarbitrary . gangSize

gvector :: Arbitrary a => Gang -> Gen [a]
gvector = vector . gangSize

gdist :: (Arbitrary a, DT a) => Gang -> Gen (Dist a)
gdist g = sized $ \n -> resize (n `div` gangSize g + 1) $ toD g `fmap` gvector g

vtype :: Gen [a] -> a -> Gen [a]
vtype = const

gtype :: Gen (Dist a) -> a -> Gen (Dist a)
gtype = const

{-
class Eq a => SemEq a where
  (===) :: a -> a -> Bool

instance Eq a => SemEq a where
  x === y | isBottom x = isBottom y
          | isBottom y = False
          | otherwise  = x == y
-}

