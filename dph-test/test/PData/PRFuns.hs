{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import Testsuite

import Data.Array.Parallel.PArray.PData.Base   as PA
import Data.Array.Parallel.PArray.PData.Scalar as PA
import Data.Array.Parallel.PArray.PData.Nested as PA
import Prelude as P
import Text.PrettyPrint
import Debug.Trace

$(testcases [ ""         <@ [t| ( Int, 
                                  PArray Int, 
                                  PArray (PArray Int),
                                  PArray (PArray (PArray Int)) )|]
            ]
  [d|
  prop_toFromList :: (PR a, Eq a) => [a] -> Bool
  prop_toFromList xs =
    toListPR (fromListPR xs) == xs
  |])


instance (Eq a, PR a)        => Eq (PArray a) where
 (==) xs ys = toListPA xs == toListPA ys

instance (PprPhysical (PArray a), Arbitrary a, PR a) => Arbitrary (PArray a) where
 arbitrary 
  = sized $ \size 
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2) $ fromIntegral size) $ arbitrary 
--        trace   (render $ pprp $ fromListPA xs) $
        return  $ fromListPA xs
