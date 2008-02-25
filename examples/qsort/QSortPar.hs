        {-# GHC_OPTIONS -fglasgow-exts #-}
--
-- TODO:
--   permute operations, which are fairly important for this algorithm, are currently
--   all sequential

module QSortPar (qsortPar)
where


import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted
import Debug.Trace

-- I'm lazy here and use the lifted qsort instead of writing a flat version
qsortPar :: UArr Double -> Int
{-# NOINLINE qsortPar #-}
qsortPar = lengthSU . qsortLifted . singletonSU


-- Remove the trivially sorted segments
qsortLifted:: SUArr Double -> SUArr Double
qsortLifted xssArr = splitApplySUP flags qsortLifted' id xssArr
  where
    flags = mapUP ((> 1)) $ lengthsSU xssArr

-- Actuall sorting
qsortLifted' xssarr = 
  if (xssLen == 0) 
    then xssarr
    else appendSU (takeCU xssLen sorted) (appendSU equal  (dropCU xssLen sorted))

  where 
  
    xssLen     = lengthSU xssarr
    xsLens     = lengthsSU xssarr
    pivots     = xssarr !:^ mapUP (flip div 2) xsLens
    pivotss    = replicateSUP xsLens pivots
    xarrLens   = zipSU xssarr pivotss 
    sorted     = qsortLifted (smaller +:+^ greater)
    smaller =  fstSU $ filterSUP (uncurryS (<)) xarrLens
    greater =  fstSU $ filterSUP (uncurryS (>)) xarrLens
    equal   =  fstSU $ filterSUP (uncurryS (==)) xarrLens



splitApplySUP:: (UA e, UA e', Show e, Show e') =>  
  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySUP #-}
splitApplySUP  flags f1 f2 xssArr = 
  if (lengthSU xssArr == 0)
    then segmentArrU emptyU emptyU 
    else combineCU flags res1 res2

  where 
    res1 = f1 $ packCUP flags xssArr 
    res2 = f2 $ packCUP (mapUP not flags) xssArr
   

appendSU:: (UA e, Show e) => SUArr e -> SUArr e -> SUArr e
{-# INLINE appendSU #-}
appendSU xssArr1 xssArr2 = segmentArrU newLengths flatData 
  where
    len        = lengthSU xssArr1 + lengthSU xssArr2
    flags      = mapUP even $ enumFromToUP 0 (len-1)
    flatData   = flattenSU $ combineCU flags xssArr1 xssArr2 
    newLengths = zipWithU (+) (lengthsSU xssArr1) (lengthsSU xssArr2)




