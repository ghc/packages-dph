        {-# GHC_OPTIONS -fglasgow-exts #-}
--
-- TODO:
--   why is combineSS slower?
module QSortSeq (qsortSeq, qsortList)
where

import Data.Array.Parallel.Unlifted

{-
qsortTest = 
--  (qsortList ([1..200000])) !!199999
  lengthSU $ qsortLifted $ toSU ([[1..200000]]::[[Double]])

qsort:: UArr Double -> UArr Double
qsort xsarr
  | xsLen < 2 = xsarr
  | otherwise = smallerEq +:+ greater
  where
    xsLen = lengthU xsarr
    pivot = xsarr !: (xsLen `div` 2)
    smallerEq = filterU (<= pivot) xsarr
    greater   = filterU (> pivot) xsarr
-}

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
    else appendSU (takeCU xssLen sorted) (appendSU equal  (dropCU xssLen sorted))
  where
    xssLen     = lengthSU xssarr
    xsLens     = lengthsSU xssarr
    pivots     = xssarr !:^ mapU (flip div 2) xsLens
    pivotss    = replicateSU xsLens pivots
    xarrLens   = zipSU xssarr pivotss 
    sorted     = qsortLifted $ (mapSU fstS $ filterSU (uncurryS (>)) xarrLens)  +:+^    
                               (mapSU fstS $ filterSU (uncurryS (<)) xarrLens) 

    equal      =               mapSU fstS $ filterSU (uncurryS (==)) xarrLens

    



packCU:: (UA e) => UArr Bool -> SUArr e -> SUArr e
{-# INLINE packCU #-}
packCU flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = flattenSU $ replicateSU (lengthsSU xssArr) flags
    flatData   = packU (flattenSU xssArr) repFlags 
    newLengths = packU (lengthsSU xssArr) flags    


      
splitApplySU:: (UA e, UA e') =>  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySU #-}
splitApplySU  flags f1 f2 xssArr = combineCU flags res1 res2
  where
    res1 = f1 $ packCU flags xssArr 
    res2 = f2 $ packCU (mapU not flags) xssArr
   

appendSU:: (UA e) => SUArr e -> SUArr e -> SUArr e
{-# INLINE appendSU #-}
appendSU xssArr1 xssArr2 = segmentArrU newLengths flatData 
  where
    len        = lengthSU xssArr1 + lengthSU xssArr2
    flags      = mapU even $ enumFromToU 0 (len-1)
    flatData   = flattenSU $ combineCU flags xssArr1 xssArr2 
    newLengths = zipWithU (+) (lengthsSU xssArr1) (lengthsSU xssArr2)




qsortList:: [Double] -> Int
qsortList = length . qsortList'

qsortList' [] = []
qsortList' xs = (qsortList' smaller) ++ equal ++ (qsortList' greater) 
  where
    p = xs !! (length xs `div` 2)
    smaller = [x | x <- xs, x < p]
    equal   = [x | x <- xs, x == p]
    greater = [x | x <- xs, x > p]
