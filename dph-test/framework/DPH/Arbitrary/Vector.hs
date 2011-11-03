
module DPH.Arbitrary.Vector where
import Test.QuickCheck
import qualified Data.Vector.Unboxed    as U
import qualified Data.Vector            as V


instance Arbitrary a => Arbitrary (V.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ V.fromList xs


instance (U.Unbox a, Arbitrary a) => Arbitrary (U.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ U.fromList xs
