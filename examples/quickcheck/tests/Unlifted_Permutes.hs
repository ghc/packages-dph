import Testsuite

import Data.Array.Parallel.Unlifted as U
import Prelude as P
import Data.List ( sort )

$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            , "acc"     <@ [t| ( Int       ) |]
            , "num"     <@ [t| ( Int       ) |]
            , "ord"     <@ [t| ( Bool, Int ) |]
            , "enum"    <@ [t| ( Bool, Int ) |]
            ]
  [d|
  prop_permute :: (Elt a, Eq a) => Array a -> Perm -> Property
  prop_permute arr (Perm is) = (U.length arr == U.length is)
    ==> permute arr is == U.map (arr!:) is
                  
  prop_bpermute :: (Elt a, Eq a) => Array a -> BPerm -> Property
  prop_bpermute arr (BPerm is) =
    (is == empty) || (maximum $ toList is) < U.length arr
    ==> bpermute arr is == U.map (arr!:) is

  prop_mbpermute :: (Elt a, Eq b, Elt b) => (a -> b) -> Array a -> BPerm -> Property
  prop_mbpermute f arr (BPerm is) =
    (is == empty) || (maximum $ toList is) < U.length arr
    ==> mbpermute f arr is == U.map (\i -> f (arr!:i)) is

  prop_bpermuteDft :: (Elt a, Eq a) => Len -> (Int -> a) -> DftPerm a -> Property
  prop_bpermuteDft (Len n) init (DftPerm pairs) =
    (pairs == empty) || (maximum (toList $ fsts pairs) < n)
    ==> toList (bpermuteDft n init pairs) == update (P.map init [0..n-1]) (toList pairs)
    where set xs i x = (P.take i xs) ++ [x] ++ (P.drop (i+1) xs)
       -- update :: [a] -> [(Int, a)] -> [a]
          update xs []         = xs
          update xs ((i,x):ps) = update (set xs i x) ps
 |])

