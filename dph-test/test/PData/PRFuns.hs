{-# LANGUAGE UndecidableInstances #-}

import DPH.Arbitrary
import DPH.Testsuite

import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray               (PA)
import Data.Array.Parallel.PArray.PData         (PArray(..), PData, PDatas, PR(..))
import Data.Array.Parallel.PArray.PData.Base    ()

import Data.Array.Parallel.PArray.PData.Nested
        ( concatPR,  concatlPR
        , unconcatPR
        , appendlPR
        , unsafeFlattenPR)

import Text.PrettyPrint                         as T
import GHC.Exts
import Control.Monad
import Data.Vector                              (Vector)
import Prelude                                  as P
import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.PArray     as PA
import qualified DPH.Operators.List             as L

-- NOTE:
-- The 'b' element type contains one less level of nesting compared with the
-- 'a' type. We use 'b' when we're checking properties of functions that
--  already require nested arrays, such as "concat".

{-
$(testcases [ ""        <@ [t|  (  Int,        PArray Int,        PArray (PArray Int)
                                ,  (),         PArray ()
                                ,  (Int, Int), PArray (Int, Int), PArray (PArray Int, PArray Int)) |]

            , "b"       <@ [t|  ( Int,         PArray Int ) |]
            ]
-}


$(testcases [ ""        <@ [t|  PArray Int |]
            , "b"       <@ [t|  PArray Int |]
            , "c"       <@ [t|  Int |]
            ]

  [d|
  -- PR Dictionary functions --------------------------------------------------
  -- All the functions defined in the PR dictionary should be tested in this
  -- section. The functions appear in the same order as in the class


  -- Converting arrays to and from lists.
  --  * If this doesn't work then we'll be generating invalid arbitrary arrays
  --    for subsequent tests.
  --  * Note that converting a nested array to and from a list is more involved
  --    than converting a flat array, as we need to construct segment descriptors.
  prop_toFromVector :: (PR a, Eq a) => Vector a -> Bool
  prop_toFromVector vec
   =  let arr    = fromVectorPR vec
      in  validPR arr 
       && vec == toVectorPR arr


  -- | Check that the arbitrary arrays we're getting are valid.
  ---  * The arbitrary instance constructs arrays by using other array operators
  --     so if they're broken the all the subseqent tests will fail as well.
  prop_valid    :: PR a => PData a -> Bool
  prop_valid pdata      
        = validPR pdata

{-
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


  ---------------------------------------------------------
  -- TODO: indexl
  ---------------------------------------------------------


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


  ---------------------------------------------------------
  -- TODO: extracts_n, extract from multiple vectors
  ---------------------------------------------------------


  -- | Append two arrays.  
  prop_append :: (PR a, Eq a) => Vector a -> Vector a -> Bool
  prop_append xs ys
    = let vec'   = xs V.++ ys
          pdata' = fromVectorPR xs `appendPR` fromVectorPR ys

      in  validPR pdata'
       && vec' == toVectorPR pdata'


  ---------------------------------------------------------
  -- TODO: appends, segmented append
  ---------------------------------------------------------


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


  -- Derived Functions --------------------------------------------------------
  -- These are PR functions that are not in the PR dictionary.
  --
-}  
  -- | Concatenate arrays
  prop_concat
        :: (PR b, PA b, Eq b)
        => VVector b -> Bool
  prop_concat (VVector vec)
   = let  vec'   = V.concat (V.toList vec)

          pdata  = fromVectorPR (V.map PA.fromVector vec)
          pdata' = concatPR pdata
          
     in   validPR pdata'
      &&  vec' == toVectorPR pdata'


  -- | Lifted concat
  prop_concatl
        :: (PR c, PA c, Eq c)
        => VVVector c  -> Property
  prop_concatl (VVVector vec)
   =  V.length vec >= 1
    ==> let vec'   = V.map join vec
         
            pdata   = fromVectorPR 
                    $ V.map PA.fromVector
                    $ V.map (V.map PA.fromVector) vec

            pdata' = concatlPR pdata
         
        in  validPR pdata'
         && (V.map PA.fromVector vec') == toVectorPR pdata'


  -- | Concat then unconcat
  prop_concat_unconcat 
        :: (PR b, PA b, Eq b)
        => VVector b -> Bool
  prop_concat_unconcat (VVector vec)
   = let  pdata   = fromVectorPR $ V.map PA.fromVector vec
          pdata'  = concatPR pdata
          
          pdata'' = unconcatPR pdata pdata'
     in   validPR pdata''
      &&  toVectorPR pdata == toVectorPR pdata''


  -- | Lifted append
  prop_appendl
        :: (PR b, PA b, Eq b)
        => VVector b -> VVector b -> Bool
  prop_appendl (VVector vec1) (VVector vec2)
   = let  -- Ensure both input vectors have the same length, 
          --   which will be the lifting context.
          len   = min (V.length vec1) (V.length vec2)
          vec1' = V.take len vec1
          vec2' = V.take len vec2
          
          -- Lifted append directly on the vectors.
          vec'   = V.map PA.fromVector $ V.zipWith (V.++) vec1' vec2'

          -- Lifted append via a nested array.
          pdata1 = fromVectorPR (V.map PA.fromVector vec1')
          pdata2 = fromVectorPR (V.map PA.fromVector vec2')
          pdata' = appendlPR pdata1 pdata2
          
     in  validPR pdata'
      && vec' == toVectorPR pdata'


  ---------------------------------------------------------
  -- TODO: slicelPD
  ---------------------------------------------------------

  
  |])


-- TODO: shift this to D.A.P.BasePretty
instance (PprPhysical a, PprPhysical b)
        => PprPhysical (a, b) where
 pprp (x, y)
  = vcat
        [ text "Tuple2"
        , T.nest 4 $ pprp x
        , T.nest 4 $ pprp y]

-- Arbitrary PArrays ----------------------------------------------------------
instance (PprPhysical (PArray a), Arbitrary a, PR a) 
       => Arbitrary (PArray a) where
 arbitrary 
  = do  plan    <- arbitrary
        pdata   <- arbitraryPDataFromExp plan
        return  $ wrapPDataAsPArray pdata


-- Arbitrary PData ------------------------------------------------------------
instance (PprPhysical (PData a), Arbitrary a, PR a) 
       => Arbitrary (PData a) where
 arbitrary 
  = do  plan    <- arbitrary
        arbitraryPDataFromExp plan
        

-- Exp ------------------------------------------------------------------------
-- | Generate a plan for building an arbitrary array.
--    If we create an array directly from a list, then the internal structure 
--    is simpler than if it had been constructed by appending or concatenating
--    several other arrays. In our tests, we want to use arrays with complicated
--    internal structure, as these have more change of showing up bugs.
--
--   We split the plan generation from the actual array, so we can check
--   that the plan is covering the cases we want. We want arrays to be build
--   from a good mixture of different operators.
--   
data Exp a
        -- Generate a flat array of the given size.
        = XArbitrary Int

        -- Append two arbitrary arrays.
        | XAppend    (Exp a) (Exp a)

        -- Concatenate a list of arbitrary arrays.
        | XConcat    [Exp a]

deriving instance 
        (Show a, Show (PData a), Show (PDatas a)) 
        => Show (Exp a)

instance Arbitrary (Exp a) where
 arbitrary
  = sized $ \s -> 
    let aFlat   
         = do   n       <- choose (0, s)
                return (XArbitrary n)

        aAppend
         = do   liftM2 XAppend 
                         (resize (s `div` 2) arbitrary)
                         (resize (s `div` 2) arbitrary)

        aConcat
         = do   n       <- choose (1, min 5 s)
                liftM XConcat
                        (vectorOf n $ resize (s `div` n) arbitrary)
                        
   in   -- If the size is small then just use a flat arary without invoking a
        -- more complex operator. This allows our properties to test those 
        -- operators in isolation, before the array structure gets too
        -- complicated.
        if s <= 10
           then aFlat
           else oneof [aFlat, aAppend, aConcat]
  

-- | Generate some PData by using the operators described by the given plan.
arbitraryPDataFromExp :: (Arbitrary a, PR a) => Exp a -> Gen (PData a)
arbitraryPDataFromExp xx
 = sized $ \s -> 
   case xx of
        XArbitrary n
         ->     arbitraryFlatPData 

        XAppend exp1 exp2
         -> do  pdata1  <- arbitraryPDataFromExp exp1
                pdata2  <- arbitraryPDataFromExp exp2
                return  $ appendPR pdata1 pdata2

        XConcat exps
         -> do  pdatas  <- mapM arbitraryPDataFromExp exps

                return  $ concatPR 
                        $ fromVectorPR $ V.fromList
                        $ map wrapPDataAsPArray pdatas


-- | Generate an arbitrary PData just by converting a list.
--   The internal representation will only contain a single physical vector.
arbitraryFlatPData :: (Arbitrary a, PR a) => Gen (PData a)
arbitraryFlatPData
  =  sized $ \s
  -> do xs      <- resize (truncate $ (\x -> sqrt x * 2 ) $ fromIntegral s) 
                $ arbitrary 

        return  $ fromVectorPR xs


wrapPDataAsPArray :: PR a => PData a -> PArray a
wrapPDataAsPArray pdata
 = let  !(I# n#)        = lengthPR pdata
   in   PArray n# pdata