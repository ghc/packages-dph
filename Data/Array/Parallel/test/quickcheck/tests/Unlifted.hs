import Testsuite

import Data.Array.Parallel.Unlifted

types = ["()", "Char", "Int"]

instance (UA a, Arbitrary a) => Arbitrary (UArr a) where
  arbitrary = fmap toU arbitrary
  coarbitrary = coarbitrary . fromU

$(testcases [t| ( (), Char, Int ) |]
  [d|
  prop_from_to :: (Eq a, UA a) => [a] -> Bool
  prop_from_to xs = fromU (toU xs) == xs

{-  
  prop_empty :: (Eq a, UA a) => a -> Bool
  prop_empty (_ :: a) = fromU emptyU == ([] :: [a])
-}

  prop_length :: UA a => UArr a -> Bool
  prop_length arr = lengthU arr  == length (fromU arr)
  
  prop_null :: UA a => UArr a -> Bool
  prop_null arr = nullU arr == (lengthU arr == 0)
  
  prop_index :: (Eq a, UA a) => UArr a -> Len -> Property
  prop_index arr (Len i) =
    i < lengthU arr
    ==> (arr !: i) == (fromU arr !! i)
  
  prop_append :: (Eq a, UA a) => UArr a -> UArr a -> Bool
  prop_append arr brr =
    fromU (arr +:+ brr) == fromU arr ++ fromU brr
  
  prop_replicate :: (Eq a, UA a) => Len -> a -> Bool
  prop_replicate (Len n) x =
    fromU (replicateU n x) == replicate n x
  
  prop_slice :: (Eq a, UA a) => UArr a -> Len -> Len -> Property
  prop_slice arr (Len i) (Len n) =
    i <= lengthU arr && n <= lengthU arr - i
    ==> fromU (sliceU arr i n) == take n (drop i $ fromU arr)
  
  prop_extract :: (Eq a, UA a) => UArr a -> Len -> Len -> Property
  prop_extract arr (Len i) (Len n) =
    i <= lengthU arr && n <= lengthU arr - i
    ==> fromU (extractU arr i n) == take n (drop i $ fromU arr)
  
  prop_take :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_take (Len n) arr =
    n <= lengthU arr
    ==> fromU (takeU n arr) == take n (fromU arr)
  
  prop_drop :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_drop (Len n) arr =
    n <= lengthU arr
    ==> fromU (dropU n arr) == drop n (fromU arr)
  
  prop_splitAt :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_splitAt (Len n) arr =
    n <= lengthU arr
    ==> let (brr, crr) = splitAtU n arr
        in (fromU brr, fromU crr) == splitAt n (fromU arr)
  
  prop_reverse :: (Eq a, UA a) => UArr a -> Bool
  prop_reverse arr =
    fromU (reverseU arr) == reverse (fromU arr)
  
  prop_map :: (UA a, Eq b, UA b) => (a -> b) -> UArr a -> Bool
  prop_map f arr =
    fromU (mapU f arr) == map f (fromU arr)
  
  prop_filter :: (Eq a, UA a) => (a -> Bool) -> UArr a -> Bool
  prop_filter f arr =
    fromU (filterU f arr) == filter f (fromU arr)
  
  prop_loopU_replicateU :: (UA e, Eq acc, Eq e', UA e')
               => LoopFn acc e e' -> acc -> Len -> e -> Bool
  prop_loopU_replicateU em start (Len n) v =
      loopU em start (replicateU n v) ==
      loopU (\a _ -> em a v) start (replicateU n noAL)
  
  {- prop_fusion2 :: (Eq acc1, Eq acc2, Eq e1, Eq e2, Eq e3,
                   UA e1, UA e2, UA e3)
               => LoopFn acc1 e1 e2 -> LoopFn acc2 e2 e3
               -> acc1 -> acc2 -> UArr e1 -> Bool
  prop_fusion2 em1 em2 start1 start2 arr =
    loopU em2 start2 (loopArr (loopU em1 start1 arr)) ==
      let
        em (acc1 :*: acc2) e = 
          case em1 acc1 e of
  	  (acc1' :*: Nothing) -> ((acc1' :*: acc2) :*: Nothing)
  	  (acc1' :*: Just e') ->
  	    case em2 acc2 e' of
  	      (acc2' :*: res) -> ((acc1' :*: acc2') :*: res)
      in
      loopSndAcc (loopU em (start1 :*: start2) arr)
  -}
  |])

  
