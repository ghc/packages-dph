import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  -- if this doesn't work nothing else will, so run this first
  prop_fromU_toU :: (Eq a, UA a) => [a] -> Bool
  prop_fromU_toU xs = fromU (toU xs) == xs

  -- Basic operations
  -- ----------------

  prop_lengthU :: UA a => UArr a -> Bool
  prop_lengthU arr = lengthU arr  == length (fromU arr)
  
  prop_nullU :: UA a => UArr a -> Bool
  prop_nullU arr = nullU arr == (lengthU arr == 0)
  
  prop_emptyU :: (Eq a, UA a) => a -> Bool
  prop_emptyU x = fromU emptyU == tail [x]

  prop_unitsU :: Len -> Bool
  prop_unitsU (Len n) =
    fromU (unitsU n) == replicate n ()

  prop_replicateU :: (Eq a, UA a) => Len -> a -> Bool
  prop_replicateU (Len n) x =
    fromU (replicateU n x) == replicate n x

  prop_indexU :: (Eq a, UA a) => UArr a -> Len -> Property
  prop_indexU arr (Len i) =
    i < lengthU arr
    ==> (arr !: i) == (fromU arr !! i)

  prop_appendU :: (Eq a, UA a) => UArr a -> UArr a -> Bool
  prop_appendU arr brr =
    fromU (arr +:+ brr) == fromU arr ++ fromU brr

  -- Subarrays
  -- --------- 

  prop_sliceU :: (Eq a, UA a) => UArr a -> Len -> Len -> Property
  prop_sliceU arr (Len i) (Len n) =
    i <= lengthU arr && n <= lengthU arr - i
    ==> fromU (sliceU arr i n) == take n (drop i $ fromU arr)
  
  prop_extractU :: (Eq a, UA a) => UArr a -> Len -> Len -> Property
  prop_extractU arr (Len i) (Len n) =
    i <= lengthU arr && n <= lengthU arr - i
    ==> fromU (extractU arr i n) == take n (drop i $ fromU arr)
  
  prop_takeU :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_takeU (Len n) arr =
    n <= lengthU arr
    ==> fromU (takeU n arr) == take n (fromU arr)
  
  prop_dropU :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_dropU (Len n) arr =
    n <= lengthU arr
    ==> fromU (dropU n arr) == drop n (fromU arr)
  
  prop_splitAtU :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_splitAtU (Len n) arr =
    n <= lengthU arr
    ==> let (brr, crr) = splitAtU n arr
        in (fromU brr, fromU crr) == splitAt n (fromU arr)

  -- Permutations
  -- ------------

  -- missing: permuteU
  -- missing: bpermuteU
  -- missing: bpermuteDftU
  
  prop_reverseU :: (Eq a, UA a) => UArr a -> Bool
  prop_reverseU arr =
    fromU (reverseU arr) == reverse (fromU arr)

  -- Higher-order operations
  -- -----------------------
  
  prop_mapU :: (UA a, Eq b, UA b) => (a -> b) -> UArr a -> Bool
  prop_mapU f arr =
    fromU (mapU f arr) == map f (fromU arr)

  -- missing: zipWithU
  -- missing: zipWith3U
  
  prop_filterU :: (Eq a, UA a) => (a -> Bool) -> UArr a -> Bool
  prop_filterU f arr =
    fromU (filterU f arr) == filter f (fromU arr)

  prop_foldlU :: (UA a, Eq b) => (b -> a -> b) -> b -> UArr a -> Bool
  prop_foldlU f z arr =
    foldlU f z arr == foldl f z (fromU arr)

  prop_foldl1U :: (UA a, Eq a) => (a -> a -> a) -> UArr a -> Property
  prop_foldl1U f arr =
    not (nullU arr)
    ==> foldl1U f arr == foldl1 f (fromU arr)

  -- missing: foldU
  -- missing: fold1U

  prop_scanlU :: (UA a, UA b, Eq b) => (b -> a -> b) -> b -> UArr a -> Bool
  prop_scanlU f z arr =
    fromU (scanlU f z arr) == init (scanl f z (fromU arr))

  prop_scanl1U :: (UA a, Eq a) => (a -> a -> a) -> UArr a -> Property
  prop_scanl1U f arr =
    not (nullU arr)
    ==> fromU (scanl1U f arr) == init (scanl1 f (fromU arr))

  -- missing: scanU
  -- missing: scan1U
  -- missing: loopU

  -- Searching
  -- ---------
  prop_elemU :: (Eq e, UA e) => e -> UArr e -> Bool
  prop_elemU x arr =
    elemU x arr == elem x (fromU arr)

  prop_notElemU :: (Eq e, UA e) => e -> UArr e -> Bool
  prop_notElemU x arr =
    notElemU x arr == notElem x (fromU arr)

  -- Logic operations
  -- ----------------

  prop_andU :: UArr Bool -> Bool
  prop_andU arr =
    andU arr == and (fromU arr)

  prop_orU :: UArr Bool -> Bool
  prop_orU arr =
    orU arr == or (fromU arr)

  prop_anyU :: UA e => (e -> Bool) -> UArr e -> Bool
  prop_anyU f arr =
    anyU f arr == any f (fromU arr)

  prop_allU :: UA e => (e -> Bool) -> UArr e -> Bool
  prop_allU f arr =
    allU f arr == all f (fromU arr)

  -- Arithmetic operations
  -- ---------------------

  prop_sumU :: (Eq num, UA num, Num num) => UArr num -> Bool
  prop_sumU arr =
    sumU arr == sum (fromU arr)

  prop_productU :: (Eq num, UA num, Num num) => UArr num -> Bool
  prop_productU arr =
    productU arr == product (fromU arr)

  prop_maximumU :: (Ord ord, UA ord) => UArr ord -> Property
  prop_maximumU arr =
    not (nullU arr)
    ==> maximumU arr == maximum (fromU arr)

  prop_minimumU :: (Ord ord, UA ord) => UArr ord -> Property
  prop_minimumU arr =
    not (nullU arr)
    ==> minimumU arr == minimum (fromU arr)

  -- Arrays of pairs
  -- ---------------

  -- missing: zipU
  -- missing: zip3U
  -- missing: unzipU
  -- missing: unzip3U

  -- Enumerations
  -- ------------

{- FIXME: what is the semantics?
  prop_enumFromToU :: (UA enum, Enum enum, Eq enum)
                   => enum -> enum -> Bool
  prop_enumFromToU from to =
    fromU (enumFromToU from to) == enumFromTo from to

  prop_enumFromThenToU :: (UA enum, Enum enum, Eq enum)
                       => enum -> enum -> enum -> Bool
  prop_enumFromThenToU from step to =
    fromU (enumFromThenToU from step to) == enumFromThenTo from step to
-}

  -- Equality
  -- --------

  prop_eqU_1 :: (Eq a, UA a) => UArr a -> Bool
  prop_eqU_1 arr = arr == arr

  prop_eqU_2 :: (Eq a, UA a) => UArr a -> UArr a -> Bool
  prop_eqU_2 arr brr = (arr == brr) == (fromU arr == fromU brr)

  -- Fusion
  -- ------
  
  prop_loopU_replicateU :: (UA e, Eq acc, Eq e', UA e')
               => LoopFn acc e e' -> acc -> Len -> e -> Bool
  prop_loopU_replicateU em start (Len n) v =
      loopU em start (replicateU n v) ==
      loopU (\a _ -> em a v) start (unitsU n)
  
  {- FIXME: disabled - too many type variables
  prop_fusion2 :: (Eq acc1, Eq acc2, Eq e1, Eq e2, Eq e3,
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

  -- missing: segmented operations
  |])

  
