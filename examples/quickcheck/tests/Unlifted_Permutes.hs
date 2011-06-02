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

  prop_bpermute :: (Elt a, Eq a) => Array a -> Array Int -> Bool
  prop_bpermute arr ixs = 
    bpermute arr ixs' == U.map (arr!:) ixs'
    where ixs' = if U.length arr == 0 then empty -- no permutations for an empty array
                                      else U.map (`mod` (U.length arr)) ixs

  prop_mbpermute :: (Elt a, Eq b, Elt b) => (a -> b) -> Array a -> Array Int -> Bool
  prop_mbpermute f arr ixs =
    mbpermute f arr ixs' == U.map (\i -> f (arr!:i)) ixs'
    where ixs' = if U.length arr == 0 then empty -- no permutations for an empty array
                                      else U.map (`mod` (U.length arr)) ixs

  prop_bpermuteDft :: (Elt a, Eq a) => Len -> (Int -> a) -> Array (Int, a) -> Bool
  prop_bpermuteDft (Len n) init pairs =
    toList (bpermuteDft n init pairs') == update (P.map init [0..n-1]) (toList pairs')
    where
       -- update :: [a] -> [(Int, a)] -> [a]
          update xs []         = xs
          update xs ((i,x):ps) = update (set xs i x) ps
       -- set :: [a] -> Int -> a -> [a]
          set xs i x = (P.take i xs) ++ [x] ++ (P.drop (i+1) xs)

          pairs' = if n <= 0 then U.zip empty empty
                             else U.map (\(i,x) -> (i `mod` n, x)) pairs

  prop_update :: (Eq a, Elt a) => Array a -> Array (Int, a) -> Bool
  prop_update arr pairs =
    toList (U.update arr pairs') == update (toList arr) (toList pairs')
    where
       -- update :: [a] -> [(Int, a)] -> [a]
          update xs []         = xs
          update xs ((i,x):ps) = update (set xs i x) ps
       -- set :: [a] -> Int -> a -> [a]
          set xs i x = (P.take i xs) ++ [x] ++ (P.drop (i+1) xs)

          pairs' = if n <= 0 then U.zip empty empty
                             else U.map (\(i,x) -> (i `mod` n, x)) pairs
          n = U.length arr

 |])

