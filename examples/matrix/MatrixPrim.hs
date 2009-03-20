module MatrixPrim
where

import Data.Array.Parallel.Unlifted as U


mmMult  order m n =  sum_s ((toSegd (U.zip (enumFromStepLen 0 order (order*order)) (U.replicate (order*order) order))) >: 
                             (U.zipWith (*) mExp nTExp))

  where 
    mExp = repeat_c (order * order * order) (U.replicate order order) 
               (toSegd (U.zip (enumFromStepLen 0 order order) (U.replicate order order)))  m
    nTExp = U.repeat  (order * order * order) order nT
             
    nT= transposeM order n 



transposeM:: Int -> U.Array Double -> U.Array Double
transposeM order m = U.bpermute m inds
  where 
    inds = U.enumFromStepLenEach (order*order) (U.zip3 (U.enumFromTo 0 (order-1)) (U.replicate order order) (U.replicate order order))
  
