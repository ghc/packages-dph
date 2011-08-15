{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import Testsuite

import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Nested
import Text.PrettyPrint
import Debug.Trace
import Control.Monad
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
   ->  V.replicate n x
    == toVectorPA (replicatePA n x) 

  -- TODO: replicates


  prop_index :: (PR a, Eq a) => Vector a -> Property
  prop_index xs
    =   V.length xs > 0
    ==> forAll (choose (0, V.length xs - 1)) $ \ix 
    ->  xs V.! ix
     == indexPA (fromVectorPA xs) ix


  -- | Extract a single slice from a single vector.
  prop_extract :: (PR a, Eq a) => Vector a -> Property
  prop_extract xs
   =  forAll (arbitrarySliceSpec (V.length xs)) $ \(SliceSpec lenSlice ixStart) 
   -> V.slice ixStart lenSlice xs  
    == toVectorPA (extractPA (fromVectorPA xs) ixStart lenSlice)


  -- | Extract many slices from a single vector.  
  prop_extracts1 :: 
        ( PprPhysical (PArray a)
        , PprVirtual  (PArray a)
        , Show a
        , PR a, Eq a) => Vector a -> Property
  prop_extracts1 xs
   =  forAll (choose (0, 10))                    $ \sliceCount
   -> forAll ( replicateM sliceCount 
             $ arbitrarySliceSpec (V.length xs)) $ \sliceSpecs

   -> let vSliceSpecs   = V.fromList sliceSpecs
          lens          = V.map sliceSpecLen    vSliceSpecs
          starts        = V.map sliceSpecStart  vSliceSpecs
          sources       = V.replicate (V.length vSliceSpecs) 0

          vresult       = V.concat $ V.toList
                        $ V.zipWith (\len start -> V.slice start len xs)
                                lens
                                starts

          aresult       = extractsPA 
                                (V.singleton $ fromVectorPA xs)
                                (V.convert sources)
                                (V.convert starts)
                                (V.convert lens)
          
          vresult2      = toVectorPA aresult
           
      in trace  ( render
                $ vcat  [ text "\n\n"
                        , text "VECTOR:"   <+> (text $ show vresult)
                        , text "VIRTUAL:"  <+> (pprv aresult)
                        , text "PHYSICAL:" <+> (pprp aresult)])
          $ vresult == vresult2

  
  prop_app :: (PR a, Eq a) => Vector a -> Vector a -> Bool
  prop_app xs ys
    =   xs V.++ ys
     == toVectorPA (fromVectorPA xs `appPA` fromVectorPA ys) 
  
  -- TODO: packByTag
  -- TODO: combine2
  
  -- TODO: fromUArrayPR
  -- TODO: toUArrayPR
  
  |])



instance (Arbitrary a) => Arbitrary (V.Vector a) where
 arbitrary
  = do  xs      <- arbitrary
        return  $ V.fromList xs


instance (PprPhysical (PArray a), Arbitrary a, PR a) 
       => Arbitrary (PArray a) where
 arbitrary 
  = sized $ \size 
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2) $ fromIntegral size) 
                $ arbitrary 

--        trace   (render $ pprp $ fromListPA xs) $
        return  $ fromListPA xs


instance (PprPhysical (PData a), Arbitrary a, PR a) 
       => Arbitrary (PData a) where
 arbitrary 
  = sized $ \size 
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2 ) $ fromIntegral size) 
                $ arbitrary 

--       trace   (render $ pprp $ fromListPR xs) $
        return  $ fromVectorPR xs
