
module DPH.Arbitrary.Vector where
import Test.QuickCheck
import qualified Data.Vector.Unboxed    as U
import qualified Data.Vector            as V
import Control.Monad


-- Vector ---------------------------------------------------------------------
instance Arbitrary a
        => Arbitrary (V.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ V.fromList xs


-- VVector --------------------------------------------------------------------
-- | Generate some nested vectors, but adjust the size of the inner vectors
--   so the whole structure contains the number of elements proportinal
--   to the outer size parameter.
--  
--   If we generate an arbitrary (V.Vector (V.Vector a)) using the builtin 
--   instances then the total number of elements will be (size * size) instead
--   of just size.
data VVector a 
        = VVector (V.Vector (V.Vector a))
        deriving Show
        
instance Arbitrary a
        => Arbitrary (VVector a) where
 arbitrary 
  = sized $ \s -> 
  do    let s'  = truncate $ sqrt $ fromIntegral s
        xs      <- liftM V.fromList $ listOf $ resize s' $ arbitrary
        return  $ VVector xs

-- VVVector -------------------------------------------------------------------
-- | Like `VVector`, but with an additional layer of nesting.
data VVVector a 
        = VVVector (V.Vector (V.Vector (V.Vector a)))
        deriving Show
        
instance Arbitrary a
        => Arbitrary (VVVector a) where
 arbitrary 
  = sized $ \s -> 
  do    let s'  = truncate $ sqrt $ fromIntegral s
        xs      <- liftM V.fromList $ listOf $ resize s' $ arbitrary
        return  $ VVVector xs


-- UVector --------------------------------------------------------------------
instance (U.Unbox a, Arbitrary a)
        => Arbitrary (U.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ U.fromList xs


