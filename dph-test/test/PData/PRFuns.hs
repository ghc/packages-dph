{-# LANGUAGE UndecidableInstances #-}

import DPH.Arbitrary
import DPH.Testsuite

import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray               (PA)
import Data.Array.Parallel.PArray.PData         (PArray(..), PData, PR(..))
import Data.Array.Parallel.PArray.PData.Base    ()
import Data.Array.Parallel.PArray.PData.Nested  (concatPR)

import Text.PrettyPrint
import GHC.Exts
import Control.Monad
import Data.Vector                              (Vector)
import Prelude                                  as P
import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.PArray     as PA
import qualified DPH.Operators.List             as L

{-
$(testcases [ ""        <@ [t|  (  Int,        PArray Int,        PArray (PArray Int)
                                ,  (),         PArray ()
                                ,  (Int, Int), PArray (Int, Int), PArray (PArray Int, PArray Int)) |]

            , "b"       <@ [t|  ( Int,         PArray Int ) |]
            ]
-}
$(testcases [ ""        <@ [t|  PArray Int |]
            , "b"       <@ [t|  PArray Int |]
            ]

  [d|
  -- Converting arrays to and from lists.
  --  Note that converting a nested array to and from a list is fairly involved, 
  --  as we need to construct the segment descriptors.
  prop_toFromVector :: (PR a, Eq a) => Vector a -> Bool
  prop_toFromVector vec
   =  let arr    = fromVectorPR vec
      in  validPR arr 
       && vec == toVectorPR arr


  -- | Define an array that maps all indices to the same element.
  --   The array size must be > 0.
  prop_replicate :: (PR a, Eq a) => a -> Property
  prop_replicate x
   =  forAll (choose (1, 100)) $ \n
   -> let arr = replicatePR n x
      in  validPR arr 
       && V.replicate n x ==  toVectorPR arr


  -- | Segmented replicate.
  prop_replicates :: (PR a, Eq a) => Vector a -> Property
  prop_replicates vec
   =  forAll (liftM V.fromList $ vectorOf (V.length vec) (choose (0, 10))) $ \repCounts
   -> let vec'  = V.concat $ V.toList
                $ V.zipWith V.replicate repCounts vec
                
          segd  = U.lengthsToSegd $ U.fromList $ V.toList repCounts
          arr'  = replicatesPR segd (fromVectorPR vec)
      in  validPR arr'
       && vec' == toVectorPR arr'


  -- | Take a single element from an array.
  prop_index :: (PR a, Eq a) => PData a -> Property
  prop_index pdata
    =   lengthPR pdata > 0
    ==> forAll (choose (0, lengthPR pdata - 1)) $ \ix 
    ->  toVectorPR pdata V.! ix
     == indexPR pdata ix


  -- | Extract a single slice from a single array.
  prop_extract :: (PR a, Eq a) => Vector a -> Property
  prop_extract vec
    =  forAll (arbitrarySliceSpec (V.length vec)) $ \(SliceSpec ixStart lenSlice)  
    -> let vec'  = V.slice ixStart lenSlice vec
           arr'  = extractPR (fromVectorPR vec) ixStart lenSlice

       in  validPR arr'
        && vec' == toVectorPR arr'

  prop_extract' :: (PR a, Eq a) => PData a -> Property
  prop_extract' pdata
    =  forAll (arbitrarySliceSpec (lengthPR pdata)) $ \(SliceSpec ixStart lenSlice)  
    -> let vec'    = V.slice ixStart lenSlice (toVectorPR pdata)
           pdata'  = extractPR pdata ixStart lenSlice

       in  validPR pdata'
        && vec' == toVectorPR pdata'


  -- | Extract many slices from a single array.
  prop_extracts1 :: (PR a, Eq a) => PData a -> Property
  prop_extracts1 pdata
   =    lengthPR pdata > 0 
    ==> forAll (choose (1, 10)) $ \sliceCount
     -> forAll (replicateM sliceCount (arbitrarySliceSpec1 (lengthPR pdata))) $ \sliceSpecs'
     -> let sliceSpecs  = V.fromList sliceSpecs'
            lens        = V.map sliceSpecLen    sliceSpecs
            starts      = V.map sliceSpecStart  sliceSpecs
            sources     = V.replicate (V.length sliceSpecs) 0

            vec         = toVectorPR pdata
            vec'        = V.concat $ V.toList
                        $ V.zipWith (\len start -> V.slice start len vec)
                                lens
                                starts

            segd        = U.lengthsToSegd $ V.convert lens
            ssegd       = U.mkSSegd  (V.convert starts) (V.convert sources) segd
            pdata'      = extractsPR (singletondPR pdata) ssegd

        in  validPR pdata' 
         && vec' == toVectorPR pdata'


  -- | Append two arrays.  
  prop_append :: (PR a, Eq a) => Vector a -> Vector a -> Bool
  prop_append xs ys
    = let vec'   = xs V.++ ys
          pdata' = fromVectorPR xs `appendPR` fromVectorPR ys

      in  validPR pdata'
       && vec' == toVectorPR pdata'


  -- | Filter an array based on some tags.
  prop_packByTag
    :: (PR a, Eq a, Arbitrary a, Show a)
    => Len -> Vector a -> Property
  prop_packByTag (Len n) zz
   =   forAll (liftM V.fromList $ vectorOf n (choose (0, 1))) $ \tags
    -> forAll (liftM V.fromList $ vectorOf n arbitrary)       $ \vec1
    -> forAll (choose (0, 1))                                 $ \tag
    -> let vec'    = V.fromList
                   $ L.packByTag  (V.toList $ vec1 `asTypeOf` zz)
                                  (V.toList $ (tags :: Vector Tag))
                                  tag

           pdata'  = packByTagPR  (fromVectorPR vec1)
                                  (U.fromList $ V.toList tags)
                                  tag
       in  validPR pdata'
        && vec' == toVectorPR pdata'


  -- | Combine two arrays based on a selector.
  prop_combine2 
     :: (PR a, Eq a, Arbitrary a, Show a) 
     => Selector -> Vector a-> Property
  prop_combine2 (Selector vecTags) zz
   =    V.length vecTags >= 2
    ==> even (V.length vecTags)
    ==> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec1
     -> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec2
     -> let vec'        = V.fromList
                        $ L.combine2 (V.toList vecTags) 
                                     (V.toList $ vec1 `asTypeOf` zz) 
                                     (V.toList $ vec2 `asTypeOf` zz)

            sel2        = U.tagsToSel2 (U.fromList $ V.toList vecTags)
            pdata'      = combine2PR  sel2 (fromVectorPR vec1) (fromVectorPR vec2)

        in  validPR pdata'
         && vec' == toVectorPR pdata'


  -- | Concatenate arrays that have been produced via combine.
  --   When an nested array has been produced with combine, it's guaranteed to contain
  --   multiple flat data arrays in its psegdata field. By concatenating it we test
  --   that extractsPR handles this representation.
  prop_combine2_concat
     :: (PR b, PA b, Eq b, Arbitrary b, Show b) 
     => Selector -> Vector (Vector b) -> Property
  prop_combine2_concat (Selector vecTags) zz
   =    V.length vecTags >= 2
    ==> even (V.length vecTags)
    ==> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec1
     -> forAll (liftM V.fromList $ vectorOf (V.length vecTags `div` 2) arbitrary) $ \vec2
     -> let vec'        = V.fromList
                        $ L.combine2 (V.toList vecTags) 
                                     (V.toList $ vec1 `asTypeOf` zz) 
                                     (V.toList $ vec2 `asTypeOf` zz)
            vec''       = V.concat (V.toList vec')

            sel2        = U.tagsToSel2 (U.fromList $ V.toList vecTags)
            pdata'      = combine2PR sel2 
                                (fromVectorPR $ V.map PA.fromVector vec1) 
                                (fromVectorPR $ V.map PA.fromVector vec2)
            pdata''     = concatPR pdata'

        in  validPR pdata''
         && vec'' == toVectorPR pdata''


  -- | Packing an array then immediately combining it should yield the original array.
  prop_combine2_packByTag
   :: (PR a, Eq a, Arbitrary a, Show a)
   => Selector -> Vector a -> Property
  prop_combine2_packByTag (Selector vecTags) zz
   =    V.length vecTags >= 2
    ==> even (V.length vecTags)
    ==> forAll (liftM V.fromList $ vectorOf (V.length vecTags) arbitrary) $ \vec
     -> let 
            uarrTags    = U.fromList $ V.toList vecTags
            sel2        = U.tagsToSel2 uarrTags

            pdata         = fromVectorPR (vec `asTypeOf` zz)
            pdata'        = combine2PR sel2
                                (packByTagPR pdata uarrTags 0)
                                (packByTagPR pdata uarrTags 1)

        in  validPR pdata'
         && toVectorPR pdata == toVectorPR pdata'


  -- | Concatenate arrays
  prop_concat :: (PR b, PA b, Eq b) => Vector (Vector b) -> Bool
  prop_concat vec
   = let  vec'   = V.concat (V.toList vec)

          pdata  = fromVectorPR (V.map PA.fromVector vec)
          pdata' = concatPR pdata
          
     in   validPR pdata'
      &&  vec' == toVectorPR pdata'

  
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

        let pdata    = fromVectorPR xs
        let !(I# i#) = lengthPR pdata
        return  $ PArray i# pdata


instance (PprPhysical (PData a), Arbitrary a, PR a) 
       => Arbitrary (PData a) where
 arbitrary 
  = sized $ \size 
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2 ) $ fromIntegral size) 
                $ arbitrary 

        return  $ fromVectorPR xs
