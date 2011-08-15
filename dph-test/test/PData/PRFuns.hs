{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import Testsuite

import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Nested
import Text.PrettyPrint
import Debug.Trace
import qualified Data.Vector    as V
import Data.Vector              (Vector)
import Prelude                  as P

$(testcases [ ""         <@ [t| ( Int, 
                                  PArray Int, 
                                  PArray (PArray Int),
                                  PArray (PArray (PArray Int)) )|]
            ]
  [d|
  -- Converting arrays to and from lists.
  --  Note that converting a nested array to and from a list is fairly involved, 
  --  as we need to construct the segment descriptors.
  prop_toFromList :: (PR a, Eq a) => Vector a -> Bool
  prop_toFromList xs 
   =   xs
    == toVectorPA (fromVectorPA xs) 

  prop_replicate :: (PR a, Eq a) => a -> Property
  prop_replicate x
   =   forAll (choose (0, 100)) $ \n
   ->  P.replicate n x
    == toListPA (replicatePA n x) 

  -- TODO: replicates

  prop_index :: (PR a, Eq a) => [a] -> Property
  prop_index xs
    =    length xs > 0
    ==>  forAll (choose (0, length xs - 1)) $ \ix 
    ->   xs P.!! ix
     ==  indexPA (fromListPA xs) ix
    
  -- TODO: extract
  -- TODO: extracts
  
  prop_app :: (PR a, Eq a) => [a] -> [a] -> Bool
  prop_app xs ys
    =   xs ++ ys
     == toListPA (fromListPA xs `appPA` fromListPA ys) 
  
  -- TODO: packByTag
  -- TODO: combine2
  
  -- TODO: fromUArrayPR
  -- TODO: toUArrayPR
  
  |])


instance (Arbitrary a)       => Arbitrary (V.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ V.fromList xs


instance (Eq a, PR a)        => Eq (PArray a) where
 (==) xs ys = toListPA xs == toListPA ys


instance (PprPhysical (PArray a), Arbitrary a, PR a) 
       => Arbitrary (PArray a) where
 arbitrary 
  = sized $ \size 
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2) $ fromIntegral size) $ arbitrary 
--        trace   (render $ pprp $ fromListPA xs) $
        return  $ fromListPA xs


instance ( PprPhysical (PData a)
         , Arbitrary a, PR a) 
       => Arbitrary (PData a) where
 arbitrary 
  = sized $ \size 
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2) $ fromIntegral size) $ arbitrary 
--       trace   (render $ pprp $ fromListPR xs) $
        return  $ fromVectorPR xs
