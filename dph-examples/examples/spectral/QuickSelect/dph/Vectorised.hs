{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module Vectorised (quickselectPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude


{-# NOINLINE quickselectPA #-}
quickselectPA:: PArray Double -> Int -> Double 
quickselectPA xs k = qselectVect' (fromPArrayP xs) k


qselectVect':: [: Double :] -> Int -> Double 
qselectVect' xs k =
  let p  = xs !: (lengthP xs `I.div` 2)
      ls = [:x | x <- xs, x D.< p:]
  in  if   k I.< (lengthP ls)
      then qselectVect' ls k
      else
        let gs  = [:x | x <- xs, x D.> p:]
            len = lengthP xs I.- lengthP gs
        in  if   k I.>= len
            then qselectVect' gs (k I.- len)
            else p

