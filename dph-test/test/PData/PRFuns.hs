{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.PArray
import Data.Array.Parallel.PArray.PData.Base

import Testsuite
import DPH.Arbitrary.Selector
import DPH.Arbitrary.SliceSpec
import DPH.Arbitrary.Perm
import DPH.Arbitrary.Int
import qualified DPH.Operators.List             as L

import Text.PrettyPrint
import Debug.Trace
import Control.Monad
import Data.Vector                              (Vector)
import Prelude                                  as P
import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U


$(testcases [ ""        <@ [t| ( Int, PArray Int, PArray (PArray Int) ) |]
            , "b"       <@ [t| ( Int, PArray Int ) |]
            ]
  [d|
  -- Converting arrays to and from lists.
  --  Note that converting a nested array to and from a list is fairly involved, 
  --  as we need to construct the segment descriptors.
  prop_toFromVector :: (PR a, Eq a) => Vector a -> Bool
  prop_toFromVector vec
   =  let arr    = fromVectorPA vec
      in  validPA arr 
       && vec == toVectorPA arr


  -- | Define an array that maps all indices to the same element.
  prop_replicate :: (PR a, Eq a) => a -> Property
  prop_replicate x
   =  forAll (choose (0, 100)) $ \n
   -> let arr = replicatePA n x
      in  validPA arr 
       && V.replicate n x ==  toVectorPA arr


  -- TODO: replicates


  -- | Take a single element from an array.
  prop_index :: (PR a, Eq a) => PArray a -> Property
  prop_index arr
    =   lengthPA arr > 0
    ==> forAll (choose (0, lengthPA arr - 1)) $ \ix 
    ->  (toVectorPA arr) V.! ix
     == indexPA arr ix


  -- | Extract a single slice from a single array.
  prop_extract :: (PR a, Eq a) => Vector a -> Property
  prop_extract vec
    =  forAll (arbitrarySliceSpec (V.length vec)) $ \(SliceSpec ixStart lenSlice)  
    -> let vec'  = V.slice ixStart lenSlice vec
           arr'  = extractPA (fromVectorPA vec) ixStart lenSlice

       in  validPA arr'
        && vec' == toVectorPA arr'

  prop_extract' :: (PR a, Eq a) => PArray a -> Property
  prop_extract' arr
    =  forAll (arbitrarySliceSpec (lengthPA arr)) $ \(SliceSpec ixStart lenSlice)  
    -> let vec'  = V.slice ixStart lenSlice (toVectorPA arr)
           arr'  = extractPA arr ixStart lenSlice

       in  validPA arr'
        && vec' == toVectorPA arr'


  -- | Extract many slices from a single array.
  prop_extracts1 :: (PR a, Eq a) => PArray a -> Property
  prop_extracts1 arr
   =    lengthPA arr > 0 
    ==> forAll (choose (1, 10)) $ \sliceCount
     -> forAll (replicateM sliceCount (arbitrarySliceSpec1 (lengthPA arr))) $ \sliceSpecs'
     -> let sliceSpecs  = V.fromList sliceSpecs'
            lens        = V.map sliceSpecLen    sliceSpecs
            starts      = V.map sliceSpecStart  sliceSpecs
            sources     = V.replicate (V.length sliceSpecs) 0

            vec         = toVectorPA arr
            vec'        = V.concat $ V.toList
                        $ V.zipWith (\len start -> V.slice start len vec)
                                lens
                                starts

            arr'        = extractsPA    (V.singleton arr)
                                        (V.convert sources)
                                        (V.convert starts)
                                        (V.convert lens)
        in  validPA arr' 
         && vec' == toVectorPA arr'


  -- | Append two arrays.  
  prop_app :: (PR a, Eq a) => Vector a -> Vector a -> Bool
  prop_app xs ys
    = let vec'  = xs V.++ ys
          arr'  = fromVectorPA xs `appPA` fromVectorPA ys

      in  validPA arr'
       && vec' == toVectorPA arr'              

  
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
                                       
           arr'    = packByTagPA  (fromVectorPA vec1)
                                  (U.fromList $ V.toList tags)
                                  tag
       in  validPA arr'
        && vec' == toVectorPA arr'

       
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
            arr'        = combine2PA  sel2 (fromVectorPA vec1) (fromVectorPA vec2)

        in  validPA arr'
         && vec' == toVectorPA arr'


  -- | Concatenate arrays that have been produced via combine.
  --   When an nested array has been produced with combine, it's guaranteed to contain
  --   multiple flat data arrays in its psegdata field. By concatenating it we test
  --   that extractsPR handles this representation.
  prop_combine2_concat
     :: (PR b, Eq b, Arbitrary b, Show b) 
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
            arr'        = combine2PA sel2 
                                (fromVectorPA $ V.map fromVectorPA vec1) 
                                (fromVectorPA $ V.map fromVectorPA vec2)
            arr''       = concatPA arr'

        in  validPA arr''
         && vec'' == toVectorPA arr''


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

            arr         = fromVectorPA (vec `asTypeOf` zz)
            arr'        = combine2PA sel2
                                (packByTagPA arr uarrTags 0)
                                (packByTagPA arr uarrTags 1)

        in  validPA arr'
         && toVectorPA arr == toVectorPA arr'

  -- TODO: Move the compound PA funs into their own module.
  -- | Concatenate arrays
  prop_concat :: (PR b, Eq b) => Vector (Vector b) -> Bool
  prop_concat vec
   = let  vec'  = V.concat (V.toList vec)

          arr   = fromVectorPA (V.map fromVectorPA vec)
          arr'  = concatPA arr
          
     in   validPA arr'
      &&  vec' == toVectorPA arr'
  
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
