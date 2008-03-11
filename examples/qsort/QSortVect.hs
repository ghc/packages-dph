{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module QSortVect (qsortVect) where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as I



import qualified Prelude

qsortVect:: PArray Double -> PArray Double 
qsortVect xs = toPArrayP  (qsortVect' (fromPArrayP xs))

qsortVect':: [: Double :] -> [: Double :]
qsortVect' xs | lengthP xs I.<=  1 = xs
              | otherwise      = qsortVect' [:x | x <- xs, x < p:] +:+
                                            [:x | x <- xs, x < p:] +:+
                                 qsortVect' [:x | x <- xs, x > p:] 
             where p =  (xs !: (lengthP xs `I.div` 2))
