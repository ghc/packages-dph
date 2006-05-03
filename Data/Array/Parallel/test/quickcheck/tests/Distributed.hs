{-# OPTIONS -fallow-undecidable-instances #-}

import Testsuite

import Data.Array.Parallel.Distributed
import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Base.Hyperstrict

class    (Eq a, MDT a, Arbitrary a, Show a) => D a
instance (Eq a, MDT a, Arbitrary a, Show a) => D a

class    (Eq a, UA a, Arbitrary a, Show a) => U a
instance (Eq a, UA a, Arbitrary a, Show a) => U a

$(testcases [ ""        <@ [t| ( (), Bool, Char, Int, UArr (), UArr Int ) |]
            , "sc"      <@ [t| ( (), Bool, Char, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "pq"      <@ [t| ( (), Int             ) |]
            ]
  [d|
  -- if this doesn't work nothing else will, so run this first
  prop_fromD_toD :: D a => Gang -> a -> Property
  prop_fromD_toD g a =
    forAll (gvector g `vtype` a) $ \xs ->
    fromD g (toD g xs) == xs

  -- Equality
  -- --------

  prop_eqD_1 :: D a => Gang -> a -> Property
  prop_eqD_1 g a =
    forAll (gdist g `gtype` a) $ \d ->
    eqD g d d

  prop_eqD_2 :: D a => Gang -> a -> Property
  prop_eqD_2 g a =
    forAll (gdist g `gtype` a) $ \dx ->
    forAll (gdist g `gtype` a) $ \dy ->
    eqD g dx dy == (fromD g dx == fromD g dy)

  prop_neqD_1 :: D a => Gang -> a -> Property
  prop_neqD_1 g a =
    forAll (gdist g `gtype` a) $ \d ->
    not (neqD g d d)

  prop_neqD_eqD :: D a => Gang -> a -> Property
  prop_neqD_eqD g a =
    forAll (gdist g `gtype` a) $ \dx ->
    forAll (gdist g `gtype` a) $ \dy ->
    eqD g dx dy == not (neqD g dx dy)

  -- Higher-order combinators
  -- ------------------------

  prop_mapD :: (D a, D b) => Gang -> (a -> b) -> Property
  prop_mapD g f =
    forAll (gdist g) $ \d ->
    fromD g (mapD g f d) == map f (fromD g d)

  prop_zipWithD :: (D a, D b, D c) => Gang -> (a -> b -> c) -> Property
  prop_zipWithD g f =
    forAll (gdist g) $ \dx ->
    forAll (gdist g) $ \dy ->
    fromD g (zipWithD g f dx dy) == zipWith f (fromD g dx) (fromD g dy)

  prop_foldD :: D a => Gang -> (a -> a -> a) -> Property
  prop_foldD g f =
    forAll (gdist g) $ \d ->
    foldD g f d == foldl1 f (fromD g d)

  prop_scanD :: D a => Gang -> (a -> a -> a) -> a -> Property
  prop_scanD g f z =
    forAll (gdist g) $ \d ->
    let (d' :*: r) = scanD g f z d
    in fromD g d' ++ [r] == scanl f z (fromD g d)

  -- Distributed scalars
  -- -------------------

  prop_splitScalarD :: D sc => Gang -> sc -> Bool
  prop_splitScalarD g x =
    fromD g (splitScalarD g x) == replicate (gangSize g) x

  prop_andD :: Gang -> Property
  prop_andD g =
    forAll (gdist g) $ \d ->
    andD g d == and (fromD g d)

  prop_orD :: Gang -> Property
  prop_orD g =
    forAll (gdist g) $ \d ->
    orD g d == or (fromD g d)

  prop_sumD :: (D num, Num num) => Gang -> num -> Property
  prop_sumD g num =
    forAll (gdist g `gtype` num) $ \d ->
    sumD g d == sum (fromD g d)

  -- Distributed pairs
  -- -----------------

  prop_zipD :: (D pq1, D pq2) => Gang -> pq1 -> pq2 -> Property
  prop_zipD g pq1 pq2 =
    forAll (gdist g `gtype` pq1) $ \dx ->
    forAll (gdist g `gtype` pq2) $ \dy ->
    fromD g (zipD dx dy) == zipWith (:*:) (fromD g dx) (fromD g dy)

  prop_unzipD :: (D pq1, D pq2) => Gang -> pq1 -> pq2 -> Property
  prop_unzipD g pq1 pq2 =
    forAll (gdist g `gtype` (pq1 :*: pq2)) $ \d ->
    let (dx :*: dy) = unzipD d
    in
    (fromD g dx, fromD g dy) == unzip (map unpairS (fromD g d))

  prop_fstD :: (D pq1, D pq2) => Gang -> pq1 -> pq2 -> Property
  prop_fstD g pq1 pq2 =
    forAll (gdist g `gtype` (pq1 :*: pq2)) $ \d ->
    fromD g (fstD d) == map fstS (fromD g d)

  prop_sndD :: (D pq1, D pq2) => Gang -> pq1 -> pq2 -> Property
  prop_sndD g pq1 pq2 =
    forAll (gdist g `gtype` (pq1 :*: pq2)) $ \d ->
    fromD g (sndD d) == map sndS (fromD g d)

  -- Distributed arrays
  -- ------------------

  prop_splitLengthD_1 :: Gang -> Len -> Bool
  prop_splitLengthD_1 g (Len n) =
    sumD g (splitLengthD g n) == n

  -- check that the distribution is [k+1,k+1,k+1,...,k,k,k,...]
  prop_splitLengthD_2 :: Gang -> Len -> Bool
  prop_splitLengthD_2 g (Len n) =
    chk (fromD g (splitLengthD g n))
    where
      chk (l:ls) = let ns = dropWhile (==l) ls
                   in
                   null ns
                   || (all (== head ns) ns
                    && head ns == l - 1)

  prop_lengthsD :: U sc => Gang -> sc -> Property
  prop_lengthsD g x =
    forAll (gdist g `gtype` replicateU 0 x) $ \darr ->
    eqD g (lengthsD darr) (mapD g lengthU darr)

  prop_splitD :: (UA sc, Eq sc) => Gang -> UArr sc -> Bool
  prop_splitD g arr =
    foldr1 (+:+) (fromD g (splitD g arr)) == arr

  prop_joinD :: U sc => Gang -> sc -> Property
  prop_joinD g x =
    forAll (gdist g `gtype` replicateU 0 x) $ \darr ->
    joinD g darr == foldr1 (+:+) (fromD g darr)

  prop_joinD_splitD :: (UA sc, Eq sc) => Gang -> UArr sc -> Bool
  prop_joinD_splitD g arr =
    joinD g (splitD g arr) == arr

  |])

