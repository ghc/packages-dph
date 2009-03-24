module MatrixPrim
where

import Data.Array.Parallel.Unlifted as U

mmMult::Int -> U.Array Double -> U.Array Double -> U.Array Double
mmMult  order m n =
      sum_s (
           (lengthsToSegd  (U.replicate (order*order) order)) 
--         (toSegd (U.zip (U.replicate (order*order) order)                            
--                  (enumFromStepLen 0 order (order*order))))
         >: (U.zipWith (*) mExp nTExp))



  where 
    mExp = repeat_c (order * order * order) (U.replicate order order) 
                    (lengthsToSegd (U.replicate order order))
--               (toSegd (U.zip (U.replicate order order) (enumFromStepLen 0 order order)))  
               m
    nTExp = U.repeat  order (order * order * order) nT
             
    nT= transposeM order n 


--mmMultTL:: Int -> Int -> U.Array Double -> U.Array Double -> U.Array Double
--mmMultTL order noOfSubMat ms ns = 
  


transposeM:: Int -> U.Array Double -> U.Array Double
transposeM order m = U.bpermute m inds
  where 
    inds = U.enumFromStepLenEach (order*order)
                   (U.zip3 (U.enumFromTo 0 (order-1))  
                           (U.replicate order order) 
                           (U.replicate order order))
  

{-
transposeML:: Int -> Int -> U.Array Double -> U.Array Double
transposeML order noOfSubMat m = U.bpermute m inds
  where
    inds = U.enumFromStepLenEach (order*order)
                   (U.zip3 (U.enumFromTo 0 (order-1))  
                           (U.replicate order order) 
                           (U.replicate order order))
    
-}
