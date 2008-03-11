{-# GHC_OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fno-spec-constr-count #-}
--
-- TODO:
--   why is combineSS slower?
module QSortSeq (qsortSeq, qsortList)
where

import Data.Array.Parallel.Unlifted


qsortSeq :: UArr Double -> Int
{-# NOINLINE qsortSeq #-}
qsortSeq  = lengthSU . qsortLifted . singletonSU

qsortLifted:: SUArr Double -> SUArr Double
qsortLifted xssArr = splitApplySU flags qsortLifted' id xssArr
  where
    flags = mapU ((>=1)) $ lengthsSU xssArr

qsortLifted' xssarr = 
  if (xssLen == 0) 
    then xssarr
    else (takeCU xssLen sorted) ^+:+^ equal  ^+:+^ (dropCU xssLen sorted)
  where
    xssLen     = lengthSU xssarr
    xsLens     = lengthsSU xssarr
    pivots     = xssarr !:^ mapU (flip div 2) xsLens
    pivotss    = replicateSU xsLens pivots
    xarrLens   = zipSU xssarr pivotss 
    sorted     = qsortLifted $ (mapSU fstS $ filterSU (uncurryS (>)) xarrLens)  +:+^    
                               (mapSU fstS $ filterSU (uncurryS (<)) xarrLens) 
    equal      =               mapSU fstS $ filterSU (uncurryS (==)) xarrLens

    
splitApplySU:: (UA e, UA e') =>  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySU #-}
splitApplySU  flags f1 f2 xssArr = combineCU flags res1 res2
  where
    res1 = f1 $ packCU flags xssArr 
    res2 = f2 $ packCU (mapU not flags) xssArr
   

qsortList:: [Double] -> Int
qsortList = length . qsortList'

qsortList' [] = []
qsortList' xs = (qsortList' smaller) ++ equal ++ (qsortList' greater) 
  where
    p = xs !! (length xs `div` 2)
    smaller = [x | x <- xs, x < p]
    equal   = [x | x <- xs, x == p]
    greater = [x | x <- xs, x > p]
