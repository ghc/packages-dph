module Testsuite.Utils (
  Len(..), LoopFn
) where

import Test.QuickCheck
import Test.QuickCheck.Batch

import Text.Show.Functions

import Data.Array.Parallel.Base.Hyperstrict

import Data.Char
import Monad (liftM)

-- infix 4 ===

newtype Len = Len Int deriving(Eq,Ord,Enum,Show,Num)

type LoopFn acc e e' = acc -> e -> acc :*: Maybe e'

instance Arbitrary Char where
  arbitrary   = fmap chr . sized $ \n -> choose (0,n)
  coarbitrary = coarbitrary . ord

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
  arbitrary = liftM (uncurry (:*:)) arbitrary
  coarbitrary (a :*: b) = coarbitrary (a,b)

instance Arbitrary Len where
  arbitrary = fmap Len (sized $ \n -> choose (0,n))
  coarbitrary (Len n) = coarbitrary n

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing), (3, liftM Just arbitrary)]
  coarbitrary Nothing  = variant 0
  coarbitrary (Just x) = variant 1 . coarbitrary x

{-
class Eq a => SemEq a where
  (===) :: a -> a -> Bool

instance Eq a => SemEq a where
  x === y | isBottom x = isBottom y
          | isBottom y = False
          | otherwise  = x == y
-}

