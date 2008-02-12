        {-# GHC_OPTIONS -fglasgow-exts #-}
--
-- TODO:
--   why is combineSS slower?
module QSortSeq (qsort, qsortList)
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

qsort :: UArr Double -> Int
{-# NOINLINE qsort #-}
qsort = lengthSU . qsortLifted . singletonSU

qsortLifted:: SUArr Double -> SUArr Double
qsortLifted xssArr = splitApplySU flags qsortLifted' id xssArr
  where
    flags = mapU ((>=1)) $ lengthsSU xssArr

qsortLifted' xssarr = 
  if (xssLen == 0) 
    then xssarr
    else appendSU (takenCU xssLen sorted) (appendSU equal  (dropnCU xssLen sorted))
  where
    xssLen     = lengthSU xssarr
    xsLens     = lengthsSU xssarr
    pivots     = indexSU xssarr $ mapU (flip div 2) xsLens
    pivotss    = replicateSU xsLens pivots
    xarrLens   = zipSU xssarr pivotss 
    sorted     = qsortLifted $ (mapSU fstS $ filterSU (uncurryS (>)) xarrLens)  +:+^    
                               (mapSU fstS $ filterSU (uncurryS (<)) xarrLens) 

    equal      =               mapSU fstS $ filterSU (uncurryS (==)) xarrLens
--    smaller    = qsortLifted $ mapSU fstS $ filterSU (uncurryS (<)) xarrLens
    



indexSU :: (UA e) => SUArr e -> UArr Int -> UArr e
{-# INLINE indexSU #-}
indexSU sArr inds = bpermuteU (flattenSU sArr) newInds
  where
    xsLens  = lengthsSU sArr
    newInds = zipWithU (+) inds $ scanU (+) 0 xsLens    
  

filterFlagsCU:: (UA e) => UArr Bool -> SUArr e -> SUArr e
{-# INLINE filterFlagsCU #-}
filterFlagsCU flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = flattenSU $ replicateSU (lengthsSU xssArr) flags
    flatData   = packU (flattenSU xssArr) repFlags 
    newLengths = packU (lengthsSU xssArr) flags    

combineCU::  (UA e) => UArr Bool -> SUArr e -> SUArr e -> SUArr e
{-# INLINE combineCU #-}
combineCU  flags xssArr1 xssArr2 = segmentArrU newLengths flatData
  where
    newLengths = combineU  flags (lengthsSU xssArr1) (lengthsSU xssArr2)
    repFlags   = replicateSU newLengths flags
    --flatData   = combineSU  flags  xssArr1   xssArr2
    flatData   = combineU  (flattenSU repFlags) (flattenSU xssArr1)  (flattenSU xssArr2)  

      
splitApplySU:: (UA e, UA e') =>  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySU #-}
splitApplySU  flags f1 f2 xssArr = combineCU flags res1 res2
  where
    res1 = f1 $ filterFlagsCU flags xssArr 
    res2 = f2 $ filterFlagsCU (mapU not flags) xssArr
   

appendSU:: (UA e) => SUArr e -> SUArr e -> SUArr e
{-# INLINE appendSU #-}
appendSU xssArr1 xssArr2 = segmentArrU newLengths flatData 
  where
    len        = lengthSU xssArr1 + lengthSU xssArr2
    flags      = mapU even $ enumFromToU 0 (len-1)
    flatData   = flattenSU $ combineCU flags xssArr1 xssArr2 
    newLengths = zipWithU (+) (lengthsSU xssArr1) (lengthsSU xssArr2)



-- there should be a better way to implent dropn and taken
-- it makes qsort terribly slow
takenCU:: (UA e) => Int ->  SUArr e  -> SUArr e
{-# INLINE takenCU #-}
takenCU n xssArr = filterFlagsCU flags xssArr
  where
    flags = mapU (<=n) $ enumFromToU 1 (lengthSU xssArr)


dropnCU:: (UA e) => Int ->  SUArr e  -> SUArr e
{-# INLINE dropnCU #-}
dropnCU n xssArr = filterFlagsCU flags xssArr
  where
    flags = mapU (>n) $ enumFromToU 1 (lengthSU xssArr)


qsortList [] = []
qsortList xs = (qsortList smaller) ++ equal ++ (qsortList greater) 
  where
    p = xs !! (length xs `div` 2)
    smaller = [x | x <- xs, x < p]
    equal   = [x | x <- xs, x == p]
    greater = [x | x <- xs, x > p]
