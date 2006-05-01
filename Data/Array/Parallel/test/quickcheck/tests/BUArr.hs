import Testsuite

import Data.Array.Parallel.Base.BUArr
import Data.Array.Parallel.Base.Hyperstrict

instance (UAE a, Arbitrary a) => Arbitrary (BUArr a) where
  arbitrary = fmap toBU arbitrary
  coarbitrary = coarbitrary . fromBU

$(testcases [t| ( (), Bool, Char, Int ) |]
  [d|
  prop_from_to :: (Eq a, UAE a) => [a] -> Bool
  prop_from_to xs = fromBU (toBU xs) == xs
{-  
  prop_empty :: (Eq a, UAE a) => a -> Bool
  prop_empty (_ :: a) = fromBU emptyBU == ([] :: [a])
-}
 
  prop_length :: UAE a => BUArr a -> Bool
  prop_length arr = lengthBU arr == length (fromBU arr)
  
  prop_index :: (Eq a, UAE a) => BUArr a -> Len -> Property
  prop_index arr (Len i) =
    i < lengthBU arr
    ==> (arr `indexBU` i) == (fromBU arr !! i)
  
  prop_replicate :: (Eq a, UAE a) => Len -> a -> Bool
  prop_replicate (Len n) x =
    fromBU (replicateBU n x) == replicate n x

  prop_slice :: (Eq a, UAE a) => BUArr a -> Len -> Len -> Property
  prop_slice arr (Len i) (Len n) =
    i <= lengthBU arr && n <= lengthBU arr - i
    ==> fromBU (sliceBU arr i n) == take n (drop i $ fromBU arr)
  
  prop_extract :: (Eq a, UAE a) => BUArr a -> Len -> Len -> Property
  prop_extract arr (Len i) (Len n) =
    i <= lengthBU arr && n <= lengthBU arr - i
    ==> fromBU (extractBU arr i n) == take n (drop i $ fromBU arr)
  
  prop_map :: (Eq b, UAE a, UAE b) => (a -> b) -> BUArr a -> Bool
  prop_map f arr =
    fromBU (mapBU f arr) == map f (fromBU arr)
  
  {-
  prop_Foldl (f :: Int -> Int -> Int) z arr =
    foldlBU f z arr == foldl f z (fromBU arr)
  prop_Sum (arr :: BUArr Int) =
    sumBU arr == sum (fromBU arr)
  prop_Scanl (f :: Int -> Int -> Int) z arr =
    fromBU (scanlBU f z arr) == init (scanl f z (fromBU arr))
  prop_Eq1 (arr :: BUArr Int)     = arr == arr
  prop_Eq2 (arr :: BUArr Int) brr = (arr == brr) == (fromBU arr == fromBU brr)
  -}
  
  prop_fusion1 :: (UAE e, Eq acc, Eq e', UAE e')
               => LoopFn acc e e' -> acc -> Len -> e -> Bool
  prop_fusion1 mf start (Len n) v =
    loopBU mf start (replicateBU n v)
    == loopBU (\a _ -> mf a v) start (replicateBU n ())
  
  {-
  prop_fusion2 (mf1 :: LoopFn Int Int Int) (mf2 :: LoopFn Int Int Int)
               start1 start2 arr =
    loopBU mf2 start2 (loopArr (loopBU mf1 start1 arr)) ==
      let
        mf (acc1 :*: acc2) e = 
          case mf1 acc1 e of
            (acc1' :*: Nothing) -> ((acc1' :*: acc2) :*: Nothing)
  	  (acc1' :*: Just e') ->
  	    case mf2 acc2 e' of
  	      (acc2' :*: res) -> ((acc1' :*: acc2') :*: res)
      in
      loopSndAcc (loopBU mf (start1 :*: start2) arr)
  -}
  |])

